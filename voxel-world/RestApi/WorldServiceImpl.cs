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
            Blocks blocks = WorldData.Chunks[request.x, request.z].Blocks;
            var response = new ShowChunkResponse();
            response.height = WorldData.SizeInChunksX;
            response.width = WorldData.SizeInChunksZ;
            response.lines = new System.Collections.Generic.List<string>(response.height);

            for (int x = 0; x<response.height; x++)
            {
                StringBuilder strline = new StringBuilder();
                for (int y=0; y<response.width; y++)
                {
                    var block = blocks[x,request.h,y];
                    char chr;
                    if (block.BlockData > 55)
                        chr = '?';
                    else
                        chr = UiTextGraphics.BlockTypeGraphic[block.BlockData];

                    strline.Append(chr);
                }
                response.lines.Add(strline.ToString());
            }
            return response;
        }
    }
}

