using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Threading.Tasks;

namespace Sean.World
{
	/// <summary>
	/// World environment type. Integer value is saved in world settings XML, so these integer values cannot be changed without breaking existing worlds.
	/// Start at 1 so we can ensure this gets loaded properly and not defaulted to zero.
	/// </summary>
	internal enum WorldType : byte
	{
		Grass = 1,
		Winter = 2,
		Desert = 3
	}

	internal static class WorldData
	{
		static WorldData()
		{
		}

		#region Properties (Saved)
		internal static WorldType WorldType { get; set; }
		/// <summary>Original Raw Seed used to generate this world. Blank if no seed was used.</summary>
		internal static string RawSeed { get; set; }
		/// <summary>Original program version used when this world was generated.</summary>
		internal static string GeneratorVersion { get; set; }

		internal static int GameObjectIdSeq;
		internal static int NextGameObjectId
		{
			get { return System.Threading.Interlocked.Increment(ref GameObjectIdSeq); }
		}

		private static int _sizeInChunksX;
		/// <summary>Number of chunks in X direction that make up the world.</summary>
		internal static int SizeInChunksX
		{
			get { return _sizeInChunksX; }
			set
			{
				_sizeInChunksX = value;
				SizeInBlocksX = _sizeInChunksX * Chunk.CHUNK_SIZE;
			}
		}

		private static int _sizeInChunksZ;
		/// <summary>Number of chunks in Z direction that make up the world.</summary>
		internal static int SizeInChunksZ
		{
			get { return _sizeInChunksZ; }
			set
			{
				_sizeInChunksZ = value;
				SizeInBlocksZ = _sizeInChunksZ * Chunk.CHUNK_SIZE;
			}
		}

		/// <summary>Number of blocks in X direction that make up the world.</summary>
		internal static int SizeInBlocksX { get; private set; }

		/// <summary>Number of blocks in Z direction that make up the world.</summary>
		internal static int SizeInBlocksZ { get; private set; }

		#endregion

		#region Properties (Dynamic)
		/// <summary>True when the world has been completely loaded from disk for server and single player or when world has been completely received in multiplayer.</summary>
		public static bool IsLoaded { get; set; }
		public static Chunks Chunks;
		public static bool GenerateWithTrees;
		#endregion

		#region Lookup Functions
		/// <summary>Get a block using world coords.</summary>
		internal static Block GetBlock(ref Coords coords)
		{
			return Chunks[coords].Blocks[coords];
		}

        internal static Block GetBlock(ref Position position)
        {
            return GetBlock(position.X, position.Y, position.Z);
        }

		/// <summary>Get a block using world x,y,z. Use this overload to avoid constructing coords when they arent needed.</summary>
		/// <remarks>For example, this provided ~40% speed increase in the World.PropagateLight function compared to constructing coords and calling the above overload.</remarks>
		internal static Block GetBlock(int x, int y, int z)
		{
			return Chunks[x / Chunk.CHUNK_SIZE, z / Chunk.CHUNK_SIZE].Blocks[x % Chunk.CHUNK_SIZE, y, z % Chunk.CHUNK_SIZE];
		}

		/// <summary>
		/// Is this position a valid block location. Includes blocks on the base of the world even though they cannot be removed.
		/// This is because the cursor can still point at them, they can still receive light, etc.
		/// Coords/Position structs have the same method. Use this one to avoid contructing coords/position when they arent needed. Large performance boost in some cases.
		/// </summary>
		internal static bool IsValidBlockLocation(int x, int y, int z)
		{
			return x >= 0 && x < SizeInBlocksX && y >= 0 && y < Chunk.CHUNK_HEIGHT && z >= 0 && z < SizeInBlocksZ;
		}

		internal static bool IsOnChunkBorder(int x, int z)
		{
			return x % Chunk.CHUNK_SIZE == 0 || z % Chunk.CHUNK_SIZE == 0 || x % Chunk.CHUNK_SIZE == Chunk.CHUNK_SIZE - 1 || z % Chunk.CHUNK_SIZE == Chunk.CHUNK_SIZE - 1;
		}

		internal static int GetHeightMapLevel(int x, int z)
		{
			return Chunks[x / Chunk.CHUNK_SIZE, z / Chunk.CHUNK_SIZE].HeightMap[x % Chunk.CHUNK_SIZE, z % Chunk.CHUNK_SIZE];
		}

		/// <summary>Check if any of 4 directly adjacent blocks receive direct sunlight. Uses the heightmap so that the server can also use this method. If the server stored light info then it could be used instead.</summary>
		internal static bool HasAdjacentBlockReceivingDirectSunlight(int x, int y, int z)
		{
			return (x < SizeInBlocksX - 1 && GetHeightMapLevel(x + 1, z) <= y) ||
			       (x > 0 && GetHeightMapLevel(x - 1, z) <= y) ||
			       (z < SizeInBlocksZ - 1 && GetHeightMapLevel(x, z + 1) <= y) ||
			       (z > 0 && GetHeightMapLevel(x, z - 1) <= y);
		}

		internal static bool IsValidStaticItemPosition(Position position) //cannot accept position by ref here
		{
			if (!IsValidBlockLocation(position.X, position.Y, position.Z))
			{
				Console.WriteLine ("Error - Invalid item position.");
				return false;
			}
			return true;
		}
		#endregion

		#region Block Place
		/// <summary>Place a single block in the world. This will mark the block as dirty.</summary>
		/// <param name="position">position to place the block at</param>
		/// <param name="type">type of block to place</param>
		/// <param name="isMultipleBlockPlacement">Use this when placing multiple blocks at once so lighting and chunk queueing only happens once.</param>
		internal static void PlaceBlock(Position position, Block.BlockType type, bool isMultipleBlockPlacement = false)
		{
			if (!position.IsValidBlockLocation || position.Y <= 0) return;

			if (type == Block.BlockType.Air)
			{
				//if destroying a block under water, fill with water instead of air
				if (position.Y + 1 < Chunk.CHUNK_HEIGHT && GetBlock(position.X, position.Y + 1, position.Z).Type == Block.BlockType.Water) type = Block.BlockType.Water;
			}

			var chunk = Chunks[position];
            var block = WorldData.GetBlock(ref position);//position.GetBlock();

			var oldType = block.Type;
			block.Type = type; //assign the new type
			var isTransparentBlock = Block.IsBlockTypeTransparent(type);
			var isTransparentOldBlock = Block.IsBlockTypeTransparent(oldType);
			block.BlockData = (ushort)(block.BlockData | 0x8000); //mark the block as dirty for the save file "diff" logic
			chunk.Blocks[position] = block; //insert the new block
			chunk.UpdateHeightMap(ref block, position.X % Chunk.CHUNK_SIZE, position.Y, position.Z % Chunk.CHUNK_SIZE);

			if (!isTransparentBlock || type == Block.BlockType.Water)
			{
				var below = position;
				below.Y--;
				if (below.Y > 0)
				{
                    if (GetBlock(ref below).Type == Block.BlockType.Grass || GetBlock(ref below).Type == Block.BlockType.Snow)
					{
						PlaceBlock(below, Block.BlockType.Dirt, true); //dont queue with this dirt block change, the source block changing takes care of it, prevents double queueing the chunk and playing sound twice
					}
				}
			}

//			//if theres a dynamic item on top of this block then let it fall
//			foreach (var item in chunk.GameItems.Values)
//			{
//				if (!item.IsMoving && item.Coords.Xblock == position.X && item.Coords.Yblock == position.Y + 1 && item.Coords.Zblock == position.Z)
//				{
//					item.IsMoving = true;
//				}
//			}
		}

		/// <summary>Place multiple blocks in the world of the same type.</summary>
		/// <param name="startPosition">start placing blocks at</param>
		/// <param name="endPosition">stop placing blocks at</param>
		/// <param name="type">type of block to place</param>
		/// <param name="isMultipleCuboidPlacement">Use this when placing multiple cuboids at once so lighting and chunk queueing only happens once.</param>
		internal static void PlaceCuboid(Position startPosition, Position endPosition, Block.BlockType type, bool isMultipleCuboidPlacement = false)
		{
			for (var x = Math.Min(startPosition.X, endPosition.X); x <= Math.Max(startPosition.X, endPosition.X); x++)
			{
				for (var y = Math.Min(startPosition.Y, endPosition.Y); y <= Math.Max(startPosition.Y, endPosition.Y); y++)
				{
					for (var z = Math.Min(startPosition.Z, endPosition.Z); z <= Math.Max(startPosition.Z, endPosition.Z); z++)
					{
						PlaceBlock(new Position(x, y, z), type, true);
					}
				}
			}
		}
		#endregion

		#region Disk
		/// <summary>
		/// Save the world to disk. Let the caller decide if this should be in a thread because in some situations it shouldnt (ie: when loading a newly generated world the file has to be saved first).
		/// This is only called by a standalone server or a server thread running in single player. In single player the user can also manually initiate a save in which case this will be called using a Task.
		/// </summary>
		internal static void SaveToDisk()
		{
		    string WorldFileTempPath = "Saved";
			if (File.Exists(WorldFileTempPath)) File.Delete(WorldFileTempPath);

			var fstream = new FileStream(WorldFileTempPath, FileMode.Create);
			var gzstream = new GZipStream(fstream, CompressionMode.Compress);
			//GZipStream only applies compression during .Write, writing 2 bytes at a time ends up inflating it a lot. Adding this saves up to 99.3%
			var buffstream = new BufferedStream(gzstream, 65536);
			var chunkBytes = new byte[Chunk.SIZE_IN_BYTES];

			var worldSettings = WorldSettings.GetXmlByteArray();
			buffstream.Write(BitConverter.GetBytes(worldSettings.Length), 0, sizeof(int));
			buffstream.Write(worldSettings, 0, worldSettings.Length); //write the length of the world config xml

			for (var x = 0; x < SizeInChunksX; x++)
			{
				for (var z = 0; z < SizeInChunksZ; z++)
				{
					Buffer.BlockCopy(Chunks[x,z].Blocks.Array, 0, chunkBytes, 0, chunkBytes.Length);
					//Buffer.BlockCopy(Chunks[x,z].Blocks.DiffArray, 0, chunkBytes, 0, chunkBytes.Length); 'bm: this will save a diff instead, WIP
					buffstream.Write(chunkBytes, 0, chunkBytes.Length);
				}
			}
			buffstream.Flush();
			buffstream.Close();
			gzstream.Close();
			fstream.Close();
			buffstream.Dispose();
			gzstream.Dispose();
			fstream.Dispose();

//			File.Copy(Settings.WorldFileTempPath, Settings.WorldFilePath, true);
//			File.Delete(Settings.WorldFileTempPath);
		}

		/// <summary>
		/// Called from Server.Controller class only. The scenarios where we load from disk are if this is a server launching with a previously saved world
		/// or if this is a single player and the server thread is loading the previously saved world.
		/// </summary>
//		internal static void LoadFromDisk()
//		{
//			var stopwatch = new Stopwatch();
//			stopwatch.Start();
//
//			var fstream = new FileStream(Settings.WorldFilePath, FileMode.Open);
//			var gzstream = new GZipStream(fstream, CompressionMode.Decompress);
//
//			var bytesRead = 0;
//			var worldSettingsSizeBytes = new byte[sizeof(int)];
//			while (bytesRead < sizeof(int))
//			{
//				bytesRead += gzstream.Read(worldSettingsSizeBytes, bytesRead, sizeof(int) - bytesRead); //read the size of the world config xml
//			}
//			var worldSettingsBytes = new byte[BitConverter.ToInt32(worldSettingsSizeBytes, 0)];
//
//			bytesRead = 0;
//			while (bytesRead < worldSettingsBytes.Length)
//			{
//				bytesRead += gzstream.Read(worldSettingsBytes, bytesRead, worldSettingsBytes.Length - bytesRead);
//			}
//			WorldSettings.LoadSettings(worldSettingsBytes);
//
//			var chunkTotal = SizeInChunksX * SizeInChunksZ;
//			var chunkCount = 1;
//			var tasks = new Task[chunkTotal];
//			for (var x = 0; x < SizeInChunksX; x++) //loop through each chunk and load it
//			{
//				for (var z = 0; z < SizeInChunksZ; z++)
//				{
//					var chunkBytes = new byte[Chunk.SIZE_IN_BYTES];
//					bytesRead = 0;
//					while (bytesRead < chunkBytes.Length)
//					{
//						bytesRead += gzstream.Read(chunkBytes, bytesRead, chunkBytes.Length - bytesRead);
//					}
//					int x1 = x, z1 = z;
//					var task = Task.Factory.StartNew(() => LoadChunk(Chunks[x1, z1], chunkBytes));
//					tasks[chunkCount - 1] = task;
//					chunkCount++;
//				}
//			}
//			Task.WaitAll(tasks);
//			gzstream.Close();
//			fstream.Close();
//
//			stopwatch.Stop();
//			Debug.WriteLine("World load from disk time: {0}ms", stopwatch.ElapsedMilliseconds);
//
//		}

		internal static void LoadChunk(Chunk chunk, byte[] bytes)
		{
			Buffer.BlockCopy(bytes, 0, chunk.Blocks.Array, 0, bytes.Length);
			chunk.BuildHeightMap();
		}
		#endregion

	}
}
