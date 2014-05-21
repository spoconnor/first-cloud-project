using System;
using System.ServiceModel;
using voxelworld;

namespace Sean
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            Console.WriteLine ("Hello World!");

            using (ServiceHost host = new ServiceHost(typeof(WorldServiceImpl)))
            {
                host.Open();
                Console.WriteLine("Server Started");
                Console.ReadLine();
                host.Close();
            }
        }
    }
}
