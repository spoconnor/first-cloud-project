using System;

namespace voxelworld
{
    public class WorldServiceImpl : IWorldService
    {
        public WorldServiceImpl ()
        {
        }

        public string HelloWorld(string id)
        {
            return "Data " + id;
        }
    }
}

