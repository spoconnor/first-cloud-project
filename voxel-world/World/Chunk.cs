using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using OpenTK;

namespace Sean.World
{
	public class Chunk
	{
		#region Constructors
		internal Chunk(int x, int z)
		{
			Coords = new ChunkCoords(x, z);
			Blocks = new Blocks(CHUNK_SIZE, CHUNK_HEIGHT, CHUNK_SIZE);
			HeightMap = new int[CHUNK_SIZE, CHUNK_SIZE];
			Clutters = new HashSet<Clutter>();
			LightSources = new ConcurrentDictionary<int, LightSource>();
			Mobs = new HashSet<Mob>();
			GameItems = new ConcurrentDictionary<int, GameItemDynamic>();

			if (!Config.IsServer) //servers have no current need to allocate memory for light maps
			{
				SkyLightMapInitial = new byte[CHUNK_SIZE, CHUNK_HEIGHT, CHUNK_SIZE]; //takes 96kb @ 32x96x32
				ItemLightMapInitial = new byte[CHUNK_SIZE, CHUNK_HEIGHT, CHUNK_SIZE]; //takes 96kb @ 32x96x32
			}
		}
		#endregion

		#region Properties
		public const int CHUNK_SIZE = 32;
		public const int CHUNK_HEIGHT = 96;
		public const int SIZE_IN_BYTES = CHUNK_SIZE * CHUNK_HEIGHT * CHUNK_SIZE * sizeof(ushort);
		private const int CLUTTER_RENDER_DISTANCE = CHUNK_SIZE * 4;
		private const int GAME_ITEM_RENDER_DISTANCE = CLUTTER_RENDER_DISTANCE;

		public ChunkCoords Coords;
		public Blocks Blocks;

		/// <summary>Heighest level in each vertical column containing a non transparent block. Sky light does not shine through this point. Used in rendering and lighting calculations.</summary>
		public int[,] HeightMap;

		/// <summary>Distance of the chunk from the player in number of blocks.</summary>
		public double DistanceFromPlayer()
		{
			return Math.Sqrt(Math.Pow(Game.Player.Coords.Xf - Coords.WorldCoordsX, 2) + Math.Pow(Game.Player.Coords.Zf - Coords.WorldCoordsZ, 2));
		}
		
		/// <summary>
		/// The build state of this chunk. When a chunk gets built it is set to 'Built' state and then marked dirty so the vbo will then get created/recreated.
		/// When a change happens in the chunk, its build state is set to 'Queued' for it to get rebuilt. When loading the initial chunk frustum, chunks are
		/// set to QueuedInitialFrustum because they dont need to be pushed to the ChangedChunkQueue. Chunks that should be built in order in the distance are
		/// set to QueuedFar and placed on the FarChunkQueue.
		/// </summary>
		internal enum BuildState : byte
		{
			/// <summary>Chunk is not loaded.</summary>
			NotLoaded,
			/// <summary>Chunk is queued for build. It will be on the ChangedChunkQueue.</summary>
			Queued,
			/// <summary>Chunk is queued for build in the distance. It will be on the FarChunkQueue.</summary>
			QueuedFar,
			/// <summary>
			/// Chunk is queued for build as part of day/night lighting cycle. It will be on the FarChunkQueue.
			/// Useful because we can determine the reason the chunk is on the far queue.
			/// -this status could eventually have logic to just do light calcs and rebuffer stored arrays if we decide to store them
			/// </summary>
			QueuedDayNight,
			/// <summary>Chunk is queued for build as part of the initial frustum of chunks loaded before entering the world.</summary>
			QueuedInitialFrustum,
			/// <summary>Chunk is queued for build as part of the initial set of chunks outside the initial radius after entering the world.</summary>
			QueuedInitialFar,
			/// <summary>Chunk is currently building.</summary>
			Building,
			/// <summary>Chunk is built.</summary>
			Built
		}

		private volatile BuildState _chunkBuildState = BuildState.NotLoaded;
		internal BuildState ChunkBuildState
		{
			get { return _chunkBuildState; }
			set
			{
				_chunkBuildState = value;
				switch (value)
				{
					case BuildState.Queued:
						WorldHost.ChangedChunkQueue.Enqueue(this);
						WorldHost.BuildChunkHandle.Set();
						break;
					case BuildState.QueuedDayNight:
					case BuildState.QueuedFar:
					case BuildState.QueuedInitialFar:
						WorldHost.FarChunkQueue.Enqueue(this);
						WorldHost.BuildChunkHandle.Set();
						break;
					case BuildState.Built:
						if (ChunkBufferState == BufferState.VboBuffered) ChunkBufferState = BufferState.VboDirty;
						break;
					case BuildState.NotLoaded:
						ChunkBufferState = BufferState.VboNotBuffered;
						UnloadData();
						break;
				}
			}
		}

		/// <summary>
		/// The buffer state of this chunk. Refers to whether a vbo is created 'VboBuffered', needs to be created or recreated 'VboDirty' or has not yet been buffered 'VboNotBuffered'.
		/// The reason the buffer state and build state are different enums is because the chunk needs to wait to be 'Built' before it can be buffered to a vbo.
		/// </summary>
		internal enum BufferState { VboNotBuffered, VboDirty, VboBuffered }
		internal volatile BufferState ChunkBufferState = BufferState.VboNotBuffered;
		#endregion

		#region Build
		/// <summary>
		/// Queue this chunk for immediate rebuild if it is within range (ie: loaded). If this chunk was QueuedFar it will get added to the immediate chunk queue.
		/// This will potentially cause the chunk to get built twice (once on each queue), however is required because if the chunk is on the far queue for day/night lighting or another slow
		/// moving process and a change is made to it, it needs to respond quickly and not wait for the far queue. This will be rare and so is not worth checking for, also if the
		/// near queue finishes before the far queue picks it up it will get skipped anyway.
		/// </summary>
		public void QueueImmediate()
		{
			//if (ChunkBuildState == BuildState.NotLoaded || ChunkBuildState == BuildState.Queued || ChunkBuildState == BuildState.Building) return; //gm: if its 'Building' we prob need to requeue anyway or it could miss something
			if (ChunkBuildState == BuildState.NotLoaded || ChunkBuildState == BuildState.Queued) return;
			ChunkBuildState = BuildState.Queued; //adds the chunk to the immediate queue
		}

		public void BuildData()
		{
			var stopwatch = new Stopwatch();
			stopwatch.Start();
			BuildState initialState;
			lock (this) //bm: multiple threads building a chunk at once can mess up the VBOs. contention ought to be rare.
			{
				initialState = ChunkBuildState;
				ChunkBuildState = BuildState.Building;

				//these are used in frustum checks
				_shortestFaceHeightTemp = CHUNK_HEIGHT + 1;

				#region Add faces needed to fill holes below the DeepestTransparentLevel where adjacent chunks have transparent blocks on the chunk edge
				if (Coords.X > 0) //get west (left) chunk x-
				{
					var adjacentChunk = WorldData.Chunks[Coords.X - 1, Coords.Z];
					if (adjacentChunk.DeepestTransparentLevel < DeepestTransparentLevel - 1) //holes are only possible if adjacent chunk DTL is lower than DTL - 1
					{
						//loop through all edge blocks on adjacent chunk between the DTL's, for any transparent block, add the adjacent face to this chunk
						//we know we have to add it because everything at these levels in this chunk are non transparent as they are below the DTL
						for (int y = adjacentChunk.DeepestTransparentLevel; y < DeepestTransparentLevel - 1; y++)
						{
							for (int z = 0; z < CHUNK_SIZE; z++)
							{
								if (!adjacentChunk.Blocks[CHUNK_SIZE - 1, y, z].IsTransparent) continue;
								var block = Blocks[0, y, z]; //make copy to pass by ref
								AddBlockFace(ref block, Face.Left, Coords.WorldCoordsX, y, Coords.WorldCoordsZ + z);
							}
						}
					}
				}
				if (Coords.X < WorldData.SizeInChunksX - 1) //get east (right) chunk x+
				{
					var adjacentChunk = WorldData.Chunks[Coords.X + 1, Coords.Z];
					if (adjacentChunk.DeepestTransparentLevel < DeepestTransparentLevel - 1) //holes are only possible if adjacent chunk DTL is lower than DTL - 1
					{
						//loop through all edge blocks on adjacent chunk between the DTL's, for any transparent block, add the adjacent face to this chunk
						//we know we have to add it because everything at these levels in this chunk are non transparent as they are below the DTL
						for (int y = adjacentChunk.DeepestTransparentLevel; y < DeepestTransparentLevel - 1; y++)
						{
							for (int z = 0; z < CHUNK_SIZE; z++)
							{
								if (!adjacentChunk.Blocks[0, y, z].IsTransparent) continue;
								var block = Blocks[CHUNK_SIZE - 1, y, z]; //make copy to pass by ref
								AddBlockFace(ref block, Face.Right, Coords.WorldCoordsX + CHUNK_SIZE - 1, y, Coords.WorldCoordsZ + z);
							}
						}
					}
				}
				if (Coords.Z > 0) //get south (front) chunk z+
				{
					var adjacentChunk = WorldData.Chunks[Coords.X, Coords.Z - 1];
					if (adjacentChunk.DeepestTransparentLevel < DeepestTransparentLevel - 1) //holes are only possible if adjacent chunk DTL is lower than DTL - 1
					{
						//loop through all edge blocks on adjacent chunk between the DTL's, for any transparent block, add the adjacent face to this chunk
						//we know we have to add it because everything at these levels in this chunk are non transparent as they are below the DTL
						for (int y = adjacentChunk.DeepestTransparentLevel; y < DeepestTransparentLevel - 1; y++)
						{
							for (int x = 0; x < CHUNK_SIZE; x++)
							{
								if (!adjacentChunk.Blocks[x, y, CHUNK_SIZE - 1].IsTransparent) continue;
								var block = Blocks[x, y, 0]; //make copy to pass by ref
								AddBlockFace(ref block, Face.Back, Coords.WorldCoordsX + x, y, Coords.WorldCoordsZ);
							}
						}
					}
				}
				if (Coords.Z < WorldData.SizeInChunksZ - 1) //get north (back) chunk z-
				{
					var adjacentChunk = WorldData.Chunks[Coords.X, Coords.Z + 1];
					if (adjacentChunk.DeepestTransparentLevel < DeepestTransparentLevel - 1) //holes are only possible if adjacent chunk DTL is lower than DTL - 1
					{
						//loop through all edge blocks on adjacent chunk between the DTL's, for any transparent block, add the adjacent face to this chunk
						//we know we have to add it because everything at these levels in this chunk are non transparent as they are below the DTL
						for (int y = adjacentChunk.DeepestTransparentLevel; y < DeepestTransparentLevel - 1; y++)
						{
							for (int x = 0; x < CHUNK_SIZE; x++)
							{
								if (!adjacentChunk.Blocks[x, y, 0].IsTransparent) continue;
								var block = Blocks[x, y, CHUNK_SIZE - 1]; //make copy to pass by ref
								AddBlockFace(ref block, Face.Front, Coords.WorldCoordsX + x, y, Coords.WorldCoordsZ + CHUNK_SIZE - 1);
							}
						}
					}
				}
				#endregion

				for (var x = 0; x < CHUNK_SIZE; x++)
				{
					var worldX = Coords.WorldCoordsX + x;
					for (var z = 0; z < CHUNK_SIZE; z++)
					{
						var worldZ = Coords.WorldCoordsZ + z;
						//only build from the DeepestTransparentLevel - 1 upwards, needs - 1 so that faces on the top of the block below still get added to the vbo (ie: the deepest possible face to stand on in the chunk)
						//no need to loop higher then HighestNonAirLevel as its only air and nothing to render above that
						for (var y = DeepestTransparentLevel - 1; y <= HighestNonAirLevel; y++)
						{
							var block = Blocks[x, y, z];
							if (block.Type == Block.BlockType.Air) continue;

							AddBlockFace(ref block, Face.Front, worldX, y, worldZ);
							AddBlockFace(ref block, Face.Right, worldX, y, worldZ);
							AddBlockFace(ref block, Face.Top, worldX, y, worldZ);
							AddBlockFace(ref block, Face.Left, worldX, y, worldZ);
							AddBlockFace(ref block, Face.Bottom, worldX, y, worldZ);
							AddBlockFace(ref block, Face.Back, worldX, y, worldZ);
						}
					}
				}

				//note: in benchmarking, the time to loop through chunk vbos and write lists to arrays is <1% of the time to build a chunk, no point in trying to optimize it
				foreach (var chunkVbo in _chunkVbos.Where(chunkVbo => chunkVbo != null))
				{
					chunkVbo.WriteListsToArrays();
				}

				ChunkBuildState = BuildState.Built;
			}

			//monitor chunk build times, for now ignore initial chunk builds, can be easily modified
			switch (initialState)
			{
				//case BuildState.QueuedInitialFrustum: //ignore to prevent debug spam
				//case BuildState.QueuedInitialFar: //ignore to prevent debug spam
				//case BuildState.QueuedDayNight: //ignore to prevent debug spam
				case BuildState.Built: //gm: i wouldnt expect to see this status here, but now we will know (as it turns out this was showing up when there were too many auto grass changes too fast for example)
				case BuildState.Building: //gm: i wouldnt expect this status here, but now we will know
				case BuildState.NotLoaded: //gm: i wouldnt expect this status here, but now we will know
				case BuildState.Queued:
				case BuildState.QueuedFar:
					Debug.WriteLine("Chunk {0} built in {1}ms ({2})", Coords, stopwatch.ElapsedMilliseconds, initialState);
					break;
			}
		}

		/// <summary>Buffer the chunks data to a vbo so it can be rendered.</summary>
		/// <remarks>When benchmarking this method, it happens so fast quite often the stopwatch would report 0ms, so dont worry about it.</remarks>
		private void BufferData()
		{
			foreach (var chunkVbo in _chunkVbos.Where(chunkVbo => chunkVbo != null))
			{
				chunkVbo.BufferData();
			}
			_shortestFaceHeight = _shortestFaceHeightTemp;
			ChunkBufferState = BufferState.VboBuffered;
		}

		/// <summary>Remove the chunks vbo when it is outside rendering range.</summary>
		private void UnloadData()
		{
			for (var i = 0; i < _chunkVbos.Length; i++)
			{
				if (_chunkVbos[i] == null) continue;
				_chunkVbos[i].DeleteBuffers();
				_chunkVbos[i] = null;
			}
		}

		/// <summary>Decide if this block face needs to be added to a VBO and rendered. XYZ are world relative coordinates.</summary>
		private void AddBlockFace(ref Block block, Face face, int x, int y, int z)
		{
			int adjacentX = x, adjacentY = y, adjacentZ = z;

			//check if surface is adjacent to another solid texture (if so dont draw it)
			switch (face)
			{
				case Face.Right: adjacentX++; break;
				case Face.Left: adjacentX--; break;
				case Face.Top: adjacentY++; break;
				case Face.Bottom: adjacentY--; break;
				case Face.Front: adjacentZ++; break;
				default: adjacentZ--; break; //back
			}

			//todo: possible optimization, get the Block from this.Blocks array when x/z are NOT on chunk edge, would result in not looking up chunk in World.GetBlock 99% of time, but tiny bit more checks for blocks on edges, try it out some other time
			//-could accept a bool param needToCheckOverChunkEdge, the DTL logic would always pass false for it because its alrdy figured it out, so both ways get the optimization
			#region todo: none of this would need to be checked for the DTL added faces, could grab the block straight out of this chunk instead, wouldnt need to check adjacent block either
			if (!WorldData.IsValidBlockLocation(adjacentX, adjacentY, adjacentZ)) return; //adjacent block is outside the world so theres no need to render this face
			var adjacentBlock = WorldData.GetBlock(adjacentX, adjacentY, adjacentZ);
			if (!adjacentBlock.IsTransparent) return; //no need to render this face because the adjacent face is not transparent
			
			//dont render inner faces of neighboring transparent blocks of same type, makes trees hollow and doesnt draw additional water faces under water
			//improves fps by limiting the number of faces needed and looks better on weak laptop gpu's as well (note: we know the adjacent face is transparent if we get this far)
			if (block.IsTransparent && block.Type == adjacentBlock.Type) return;
			#endregion

			var texture = Block.FaceTexture(block.Type, face);
			//check if there is an existing vbo for this chunk/texture to add this face to
			if (_chunkVbos[(int)texture] == null)
			{
				//vbo not created yet for this chunk/texture so we need to start one
				_chunkVbos[(int)texture] = new ChunkVbo(ref block, TextureLoader.GetBlockTexture(texture));
			}

			//we need position 4 no matter what, so retrieve it here, position 4 is used for the lighting values of all 4 vertices of this face when smooth lighting is off
			byte lightColor = WorldData.GetBlockLightColor(adjacentX, adjacentY, adjacentZ);

			_shortestFaceHeightTemp = Math.Min(_shortestFaceHeightTemp, y);

			if (block.Type == Block.BlockType.Leaves || block.Type == Block.BlockType.SnowLeaves)
			{
				//dont bother with smooth lighting for leaves, it would rarely matter anyway
				if (face == Face.Bottom) lightColor = (byte)(lightColor * 0.6); //make bottom leaves faces darker by giving them 60% of the light they would otherwise have
				BlockRender.AddBlockFaceToVbo(_chunkVbos[(int)texture], face, x, y, z, lightColor);
				return;
			}

			if (Config.SmoothLighting)
			{
				#region Smooth Lighting
				//SMOOTH LIGHTING MAP (block 4 is used by all 4 vertices, blocks 1,3,5,7 are used by 2 vertices each, blocks 0,2,6,8 are used by one vertex only)
				//	0  1  2
				//   v1 v0
				//	3  4  5
				//   v2 v3
				//	6  7  8
				var xyz = new Position[9];
				var smoothLighting = new byte[9];
				smoothLighting[4] = lightColor; //we already have the directly adjacent color which goes in position 4
				switch (face)
				{
					case Face.Right: //X is constant
						xyz[0] = new Position(adjacentX, adjacentY + 1, adjacentZ + 1);
						xyz[1] = new Position(adjacentX, adjacentY + 1, adjacentZ);
						xyz[2] = new Position(adjacentX, adjacentY + 1, adjacentZ - 1);
						xyz[3] = new Position(adjacentX, adjacentY, adjacentZ + 1);
						xyz[5] = new Position(adjacentX, adjacentY, adjacentZ - 1);
						xyz[6] = new Position(adjacentX, adjacentY - 1, adjacentZ + 1);
						xyz[7] = new Position(adjacentX, adjacentY - 1, adjacentZ);
						xyz[8] = new Position(adjacentX, adjacentY - 1, adjacentZ - 1);
						break;
					case Face.Left: //X is constant
						xyz[0] = new Position(adjacentX, adjacentY + 1, adjacentZ - 1);
						xyz[1] = new Position(adjacentX, adjacentY + 1, adjacentZ);
						xyz[2] = new Position(adjacentX, adjacentY + 1, adjacentZ + 1);
						xyz[3] = new Position(adjacentX, adjacentY, adjacentZ - 1);
						xyz[5] = new Position(adjacentX, adjacentY, adjacentZ + 1);
						xyz[6] = new Position(adjacentX, adjacentY - 1, adjacentZ - 1);
						xyz[7] = new Position(adjacentX, adjacentY - 1, adjacentZ);
						xyz[8] = new Position(adjacentX, adjacentY - 1, adjacentZ + 1);
						break;
					case Face.Top: //Y is constant
						xyz[0] = new Position(adjacentX - 1, adjacentY, adjacentZ - 1);
						xyz[1] = new Position(adjacentX, adjacentY, adjacentZ - 1);
						xyz[2] = new Position(adjacentX + 1, adjacentY, adjacentZ - 1);
						xyz[3] = new Position(adjacentX - 1, adjacentY, adjacentZ);
						xyz[5] = new Position(adjacentX + 1, adjacentY, adjacentZ);
						xyz[6] = new Position(adjacentX - 1, adjacentY, adjacentZ + 1);
						xyz[7] = new Position(adjacentX, adjacentY, adjacentZ + 1);
						xyz[8] = new Position(adjacentX + 1, adjacentY, adjacentZ + 1);
						break;
					case Face.Bottom: //Y is constant
						xyz[0] = new Position(adjacentX - 1, adjacentY, adjacentZ + 1);
						xyz[1] = new Position(adjacentX, adjacentY, adjacentZ + 1);
						xyz[2] = new Position(adjacentX + 1, adjacentY, adjacentZ + 1);
						xyz[3] = new Position(adjacentX - 1, adjacentY, adjacentZ);
						xyz[5] = new Position(adjacentX + 1, adjacentY, adjacentZ);
						xyz[6] = new Position(adjacentX - 1, adjacentY, adjacentZ - 1);
						xyz[7] = new Position(adjacentX, adjacentY, adjacentZ - 1);
						xyz[8] = new Position(adjacentX + 1, adjacentY, adjacentZ - 1);
						break;
					case Face.Front: //Z is constant
						xyz[0] = new Position(adjacentX - 1, adjacentY + 1, adjacentZ);
						xyz[1] = new Position(adjacentX, adjacentY + 1, adjacentZ);
						xyz[2] = new Position(adjacentX + 1, adjacentY + 1, adjacentZ);
						xyz[3] = new Position(adjacentX - 1, adjacentY, adjacentZ);
						xyz[5] = new Position(adjacentX + 1, adjacentY, adjacentZ);
						xyz[6] = new Position(adjacentX - 1, adjacentY - 1, adjacentZ);
						xyz[7] = new Position(adjacentX, adjacentY - 1, adjacentZ);
						xyz[8] = new Position(adjacentX + 1, adjacentY - 1, adjacentZ);
						break;
					default: //Back; Z is constant
						xyz[0] = new Position(adjacentX + 1, adjacentY + 1, adjacentZ);
						xyz[1] = new Position(adjacentX, adjacentY + 1, adjacentZ);
						xyz[2] = new Position(adjacentX - 1, adjacentY + 1, adjacentZ);
						xyz[3] = new Position(adjacentX + 1, adjacentY, adjacentZ);
						xyz[5] = new Position(adjacentX - 1, adjacentY, adjacentZ);
						xyz[6] = new Position(adjacentX + 1, adjacentY - 1, adjacentZ);
						xyz[7] = new Position(adjacentX, adjacentY - 1, adjacentZ);
						xyz[8] = new Position(adjacentX - 1, adjacentY - 1, adjacentZ);
						break;
				}

				//need to know if blocks on positions 1,3,5,7 are transparent, each value is used twice
				//-for example neither 1 or 5 are transparent, then vertex0 should not have any influence from light at position 2 and should use the darkest value instead for a dark corner
				//		-position 2 might be on the other side of a wall and then we get sunlight coming into ceiling corners etc
				//-if the coords of any of these positions are outside the world then consider them transparent
				bool transparent1 = !xyz[1].IsValidBlockLocation || xyz[1].GetBlock().IsTransparent;
				bool transparent3 = !xyz[3].IsValidBlockLocation || xyz[3].GetBlock().IsTransparent;
				bool transparent5 = !xyz[5].IsValidBlockLocation || xyz[5].GetBlock().IsTransparent;
				bool transparent7 = !xyz[7].IsValidBlockLocation || xyz[7].GetBlock().IsTransparent;

				smoothLighting[0] = transparent1 || transparent3 ? xyz[0].LightColor : Lighting.DARKEST_COLOR;
				smoothLighting[1] = xyz[1].LightColor;
				smoothLighting[2] = transparent1 || transparent5 ? xyz[2].LightColor : Lighting.DARKEST_COLOR;
				smoothLighting[3] = xyz[3].LightColor;
				smoothLighting[5] = xyz[5].LightColor;
				smoothLighting[6] = transparent3 || transparent7 ? xyz[6].LightColor : Lighting.DARKEST_COLOR;
				smoothLighting[7] = xyz[7].LightColor;
				smoothLighting[8] = transparent5 || transparent7 ? xyz[8].LightColor : Lighting.DARKEST_COLOR;

				//average 4 colors to get the color for each vertex
				var v0Color = (byte)((smoothLighting[1] + smoothLighting[2] + smoothLighting[4] + smoothLighting[5]) / 4);
				var v1Color = (byte)((smoothLighting[0] + smoothLighting[1] + smoothLighting[3] + smoothLighting[4]) / 4);
				var v2Color = (byte)((smoothLighting[3] + smoothLighting[4] + smoothLighting[6] + smoothLighting[7]) / 4);
				var v3Color = (byte)((smoothLighting[4] + smoothLighting[5] + smoothLighting[7] + smoothLighting[8]) / 4);

				BlockRender.AddBlockFaceToVbo(_chunkVbos[(int)texture], face, x, y, z, v0Color, v1Color, v2Color, v3Color);
				#endregion
			}
			else //use simple lighting
			{
				BlockRender.AddBlockFaceToVbo(_chunkVbos[(int)texture], face, x, y, z, lightColor);
			}
		}
		#endregion

		#region Height Map
		/// <summary>Y level of the deepest transparent block in this chunk. When building the vbo, we only need to start at 1 level below this.</summary>
		internal int DeepestTransparentLevel { get; set; }

		/// <summary>Y level of the highest non air block. Improves chunk build times. Nothing is rendered higher then this so when building the chunk vbo theres no need to go any higher.</summary>
		internal int HighestNonAirLevel { get; set; }

		/// <summary>
		/// Build a heightmap for this chunk. This is the highest non transparent block in each vertical column.
		/// Leaves, water and other transparent blocks that light can shine through do not count.
		/// </summary>
		/// <remarks>The height map is used for lighting. Its also used to determine the players starting Y position.</remarks>
		internal void BuildHeightMap()
		{
			DeepestTransparentLevel = CHUNK_HEIGHT; //initialize to top of chunk until this gets calculated
			HighestNonAirLevel = 0; //initialize to bottom of chunk until this gets calculated
			for (var x = 0; x < CHUNK_SIZE; x++)
			{
				for (var z = 0; z < CHUNK_SIZE; z++)
				{
					for (var y = CHUNK_HEIGHT - 1; y >= 0; y--) //loop from the highest block position downward until we find a solid block
					{
						var block = Blocks[x, y, z];
						if (y > HighestNonAirLevel && block.Type != Block.BlockType.Air) HighestNonAirLevel = y;
						if (block.IsTransparent) continue;
						HeightMap[x, z] = y;
						break;
					}

					for (var y = 0; y < CHUNK_HEIGHT - 1; y++) //loop from the base of the world upwards until finding a transparent block
					{
						if (!Blocks[x, y, z].IsTransparent) continue;
						if (y < DeepestTransparentLevel) DeepestTransparentLevel = y; //record this as the deepest transparent level if it is deeper then what we had previously
						break;
					}
				}
			}
		}

		/// <summary>Updates the heightmap following a block placement. Usually a lot quicker then re-building the heightmap.</summary>
		internal void UpdateHeightMap(ref Block block, int chunkRelativeX, int yLevel, int chunkRelativeZ)
		{
			var currentHeight = HeightMap[chunkRelativeX, chunkRelativeZ];
			if (block.IsTransparent) //transparent block
			{
				//update height map
				if (yLevel == currentHeight)
				{
					//transparent block being placed at the previous heightmap level, most likely removing a block (which places Air), so we need to find the next non transparent block for the heightmap
					for (var y = currentHeight - 1; y >= 0; y--) //start looking down from the previous heightmap level
					{
						if (y > 0 && Blocks[chunkRelativeX, y, chunkRelativeZ].IsTransparent) continue;
						//found the next non transparent block, update the heightmap and exit
						HeightMap[chunkRelativeX, chunkRelativeZ] = y;
						break;
					}
				}

				//update deepest transparent level
				if (yLevel < DeepestTransparentLevel) DeepestTransparentLevel = yLevel;
			}
			else //non transparent block
			{
				//update height map
				//when placing a non transparent block, check if its above the current heightmap value and if so update the heightmap
				if (yLevel > currentHeight) HeightMap[chunkRelativeX, chunkRelativeZ] = yLevel;

				//update deepest transparent level
				if (yLevel == DeepestTransparentLevel)
				{
					//this block is being set at the DeepestTransparentLevel of this chunk
					//we will need to calc if this is still the deepest level (because theres another transparent block at this depth) or what the new level is
					//the easiest way to do that is just rebuild the height map, even though all we really need to do is the portion that updates the deepest level
					BuildHeightMap();
					return; //no need to continue on to check anything else when doing a full heightmap rebuild
				}
			}

			//update HighestNonAirLevel property
			//1. if placing air (removing block), is it at same level as previous HighestNonAir?, just rebuild HeightMap in this case, otherwise do nothing
			//2. if placing anything other then air, simply check if its > HighestNonAirLevel and set it
			if (block.Type == Block.BlockType.Air) //removing a block
			{
				if (yLevel == HighestNonAirLevel) BuildHeightMap();
			}
			else //adding a block
			{
				if (yLevel > HighestNonAirLevel) HighestNonAirLevel = yLevel;
			}
		}
		#endregion

		#region Frustum
		/// <summary>
		/// Shortest face height. Used in frustum checks. Calculated while building vbo's.
		/// Use for frustum logic instead of DeepestTransparentLevel because this will also account for faces drawn below the
		/// DeepestTransparentLevel to be visible from adjacent chunks.
		/// </summary>
		private int _shortestFaceHeight;
		private int _shortestFaceHeightTemp;

		/// <summary>Is this chunk in the players view frustum.</summary>
		/// <seealso cref="http://www.crownandcutlass.com/features/technicaldetails/frustum.html"/>
		internal bool IsInFrustum
		{
			get
			{
				float minX = Coords.WorldCoordsX;
				var maxX = minX + CHUNK_SIZE;
				float minZ = Coords.WorldCoordsZ;
				var maxZ = minZ + CHUNK_SIZE;

				var nfXmin = Game.NearFrustum.X * minX;
				var nfXmax = Game.NearFrustum.X * maxX;
				var nfYmin = Game.NearFrustum.Y * _shortestFaceHeight;
				var nfYmax = Game.NearFrustum.Y * HighestNonAirLevel;
				var nfZmin = Game.NearFrustum.Z * minZ;
				var nfZmax = Game.NearFrustum.Z * maxZ;

				if (nfXmin + nfYmax + nfZmin + Game.NearFrustum.W < -0.005f
					&& nfXmin + nfYmax + nfZmax + Game.NearFrustum.W < -0.005f
					&& nfXmax + nfYmax + nfZmin + Game.NearFrustum.W < -0.005f
					&& nfXmax + nfYmax + nfZmax + Game.NearFrustum.W < -0.005f
					&& nfXmin + nfYmin + nfZmin + Game.NearFrustum.W < -0.005f
					&& nfXmin + nfYmin + nfZmax + Game.NearFrustum.W < -0.005f
					&& nfXmax + nfYmin + nfZmin + Game.NearFrustum.W < -0.005f
					&& nfXmax + nfYmin + nfZmax + Game.NearFrustum.W < -0.005f) return false;

				var ffXmin = Game.FarFrustum.X * minX;
				var ffXmax = Game.FarFrustum.X * maxX;
				var ffYmin = Game.FarFrustum.Y * _shortestFaceHeight;
				var ffYmax = Game.FarFrustum.Y * HighestNonAirLevel;
				var ffZmin = Game.FarFrustum.Z * minZ;
				var ffZmax = Game.FarFrustum.Z * maxZ;

				if (ffXmin + ffYmax + ffZmin + Game.FarFrustum.W < -0.005f
					&& ffXmin + ffYmax + ffZmax + Game.FarFrustum.W < -0.005f
					&& ffXmax + ffYmax + ffZmin + Game.FarFrustum.W < -0.005f
					&& ffXmax + ffYmax + ffZmax + Game.FarFrustum.W < -0.005f
					&& ffXmin + ffYmin + ffZmin + Game.FarFrustum.W < -0.005f
					&& ffXmin + ffYmin + ffZmax + Game.FarFrustum.W < -0.005f
					&& ffXmax + ffYmin + ffZmin + Game.FarFrustum.W < -0.005f
					&& ffXmax + ffYmin + ffZmax + Game.FarFrustum.W < -0.005f) return false;

				var lfXmin = Game.LeftFrustum.X * minX;
				var lfXmax = Game.LeftFrustum.X * maxX;
				var lfYmin = Game.LeftFrustum.Y * _shortestFaceHeight;
				var lfYmax = Game.LeftFrustum.Y * HighestNonAirLevel;
				var lfZmin = Game.LeftFrustum.Z * minZ;
				var lfZmax = Game.LeftFrustum.Z * maxZ;

				if (lfXmin + lfYmax + lfZmin + Game.LeftFrustum.W < -0.005f
					&& lfXmin + lfYmax + lfZmax + Game.LeftFrustum.W < -0.005f
					&& lfXmax + lfYmax + lfZmin + Game.LeftFrustum.W < -0.005f
					&& lfXmax + lfYmax + lfZmax + Game.LeftFrustum.W < -0.005f
					&& lfXmin + lfYmin + lfZmin + Game.LeftFrustum.W < -0.005f
					&& lfXmin + lfYmin + lfZmax + Game.LeftFrustum.W < -0.005f
					&& lfXmax + lfYmin + lfZmin + Game.LeftFrustum.W < -0.005f
					&& lfXmax + lfYmin + lfZmax + Game.LeftFrustum.W < -0.005f) return false;

				var rfXmin = Game.RightFrustum.X * minX;
				var rfXmax = Game.RightFrustum.X * maxX;
				var rfYmin = Game.RightFrustum.Y * _shortestFaceHeight;
				var rfYmax = Game.RightFrustum.Y * HighestNonAirLevel;
				var rfZmin = Game.RightFrustum.Z * minZ;
				var rfZmax = Game.RightFrustum.Z * maxZ;

				if (rfXmin + rfYmax + rfZmin + Game.RightFrustum.W < -0.005f
					&& rfXmin + rfYmax + rfZmax + Game.RightFrustum.W < -0.005f
					&& rfXmax + rfYmax + rfZmin + Game.RightFrustum.W < -0.005f
					&& rfXmax + rfYmax + rfZmax + Game.RightFrustum.W < -0.005f
					&& rfXmin + rfYmin + rfZmin + Game.RightFrustum.W < -0.005f
					&& rfXmin + rfYmin + rfZmax + Game.RightFrustum.W < -0.005f
					&& rfXmax + rfYmin + rfZmin + Game.RightFrustum.W < -0.005f
					&& rfXmax + rfYmin + rfZmax + Game.RightFrustum.W < -0.005f) return false;

				var tfXmin = Game.TopFrustum.X * minX;
				var tfXmax = Game.TopFrustum.X * maxX;
				var tfYmin = Game.TopFrustum.Y * _shortestFaceHeight;
				var tfYmax = Game.TopFrustum.Y * HighestNonAirLevel;
				var tfZmin = Game.TopFrustum.Z * minZ;
				var tfZmax = Game.TopFrustum.Z * maxZ;

				if (tfXmin + tfYmax + tfZmin + Game.TopFrustum.W < -0.005f
					&& tfXmin + tfYmax + tfZmax + Game.TopFrustum.W < -0.005f
					&& tfXmax + tfYmax + tfZmin + Game.TopFrustum.W < -0.005f
					&& tfXmax + tfYmax + tfZmax + Game.TopFrustum.W < -0.005f
					&& tfXmin + tfYmin + tfZmin + Game.TopFrustum.W < -0.005f
					&& tfXmin + tfYmin + tfZmax + Game.TopFrustum.W < -0.005f
					&& tfXmax + tfYmin + tfZmin + Game.TopFrustum.W < -0.005f
					&& tfXmax + tfYmin + tfZmax + Game.TopFrustum.W < -0.005f) return false;

				var bfXmin = Game.BottomFrustum.X * minX;
				var bfXmax = Game.BottomFrustum.X * maxX;
				var bfYmin = Game.BottomFrustum.Y * _shortestFaceHeight;
				var bfYmax = Game.BottomFrustum.Y * HighestNonAirLevel;
				var bfZmin = Game.BottomFrustum.Z * minZ;
				var bfZmax = Game.BottomFrustum.Z * maxZ;

				if (bfXmin + bfYmax + bfZmin + Game.BottomFrustum.W < -0.005f
					&& bfXmin + bfYmax + bfZmax + Game.BottomFrustum.W < -0.005f
					&& bfXmax + bfYmax + bfZmin + Game.BottomFrustum.W < -0.005f
					&& bfXmax + bfYmax + bfZmax + Game.BottomFrustum.W < -0.005f
					&& bfXmin + bfYmin + bfZmin + Game.BottomFrustum.W < -0.005f
					&& bfXmin + bfYmin + bfZmax + Game.BottomFrustum.W < -0.005f
					&& bfXmax + bfYmin + bfZmin + Game.BottomFrustum.W < -0.005f
					&& bfXmax + bfYmin + bfZmax + Game.BottomFrustum.W < -0.005f) return false;

				return true;
			}
		}
		#endregion
	}
}
