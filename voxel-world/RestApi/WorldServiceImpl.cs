using System;
using System.ServiceModel;

namespace voxelworld
{
   // [ServiceBehavior(InstanceContextMode = InstanceContextMode.Single)]
    public class WorldServiceImpl : IWorldService
    {
        public WorldServiceImpl ()
        {
        }

        public string HelloWorld()
        {
            Console.WriteLine ("Received {0}");
            return "Data ";// + id;
        }
    }
}

