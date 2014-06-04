using System;

namespace Sean.World.RestApi
{

    public class WorldServiceImpl : IWorldService
    {
        public WorldServiceImpl ()
        {
        }

        public string HelloWorld(string msg)
        {
            Console.WriteLine ("Received msg {0}", msg);
            return "Hey there!";// + id;
        }

        public string GetChunkAddress(int x, int y, int z)
        {
            Console.WriteLine ("GetChunkAddress {0},{1},{2}", x,y,z);
            return "blah";
        }
        
        public bool SetBlock(int x, int y, int z, ushort block)
        {
            Console.WriteLine ("SetBlock {0}", block);
            return true;
        }
         
        public ShowChunkResponse ShowChunk(ShowChunkRequest request)
        {
            return new ShowChunkResponse() { height = 1, width = 2};
          //  int[,] heightMap = WorldData.Chunks[request.x, request.z].HeightMap;
          //  return new ShowChunkResponse();
        }
    }
}

