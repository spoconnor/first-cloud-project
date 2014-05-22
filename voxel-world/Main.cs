using System;
using System.ServiceModel;
using System.ServiceModel.Description;
using voxelworld;

namespace voxelworld
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            Console.WriteLine ("Hello World!");

//            using (ServiceHost host = new ServiceHost(typeof(WorldServiceImpl)))
//            {
//                host.Open();
//                Console.WriteLine("Server Started");
//                Console.ReadLine();
//                host.Close();
//            }

            Uri baseAddress = new Uri("http://localhost:8888/GettingStarted/");
            ServiceHost selfHost = new ServiceHost(typeof(WorldServiceImpl), baseAddress);

            try
            {
                selfHost.AddServiceEndpoint(typeof(IWorldService), new WebHttpBinding(), "WorldService");

                ServiceMetadataBehavior smb = new ServiceMetadataBehavior();
                smb.HttpGetEnabled = true;
                selfHost.Description.Behaviors.Add(smb);

                selfHost.Open();
                Console.WriteLine("The service is ready.");
                Console.WriteLine("Press <ENTER> to terminate service.");
                Console.WriteLine();
                Console.ReadLine();

                selfHost.Close();
            }
            catch (CommunicationException ce)
            {
                Console.WriteLine("An exception occurred: {0}", ce.Message);
                Console.WriteLine("Press <ENTER> to terminate service.");
                Console.WriteLine();
                Console.ReadLine();
                selfHost.Abort();
            }
        }
    }
}
