using System;
using System.Text;

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
            if (request == null)
            {
                Console.WriteLine ("Nope :(");
                return new ShowChunkResponse();
            }
            int[,] heightMap = WorldData.Chunks[request.x, request.z].HeightMap;
            var response = new ShowChunkResponse();
            response.height = heightMap.GetLength(0);
            response.width = heightMap.GetLength(1);
            response.lines = new System.Collections.Generic.List<string>(response.height);

            for (int x = 0; x<response.height; x++)
            {
                StringBuilder strline = new StringBuilder();
                for (int y=0; y<response.width; y++)
                {
                    strline.Append(Convert.ToChar(heightMap[x,y] + ' '));
                }
                response.lines.Add(strline.ToString());
            }
            return response;
        }
    }
}

