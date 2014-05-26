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
            Console.WriteLine ("Received msg");
            return "Data ";// + id;
        }
    }
}

