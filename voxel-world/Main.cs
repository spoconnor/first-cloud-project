using System;
using System.ServiceModel;
using System.ServiceModel.Description;
using voxelworld;
using System.Configuration;

namespace voxelworld
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            Console.WriteLine ("Hello World!");

            ServiceHost selfHost = new ServiceHost(typeof(WorldServiceImpl));

            try
            {
                //selfHost.AddServiceEndpoint(typeof(IWorldService), new WebHttpBinding(), "WorldService");
                //
                //ServiceMetadataBehavior smb = new ServiceMetadataBehavior();
                //smb.HttpGetEnabled = true;
                //selfHost.Description.Behaviors.Add(smb);

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
