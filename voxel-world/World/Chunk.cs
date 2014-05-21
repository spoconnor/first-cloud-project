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
		public double DistanceFromPlayer(Coords Player)
		{
			return Math.Sqrt(Math.Pow(Player.Xf - Coords.WorldCoordsX, 2) + Math.Pow(Player.Zf - Coords.WorldCoordsZ, 2));
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
