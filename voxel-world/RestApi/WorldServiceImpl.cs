using System;
using System.ServiceModel;

namespace voxelworld
{
    [ServiceBehavior(InstanceContextMode = InstanceContextMode.Single)]
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

