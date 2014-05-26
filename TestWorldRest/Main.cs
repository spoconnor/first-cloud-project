using System;
using System.ServiceModel;
using voxelworld;

namespace TestWorldRest
{
    class MainClass
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");

            //EndpointAddress address = new EndpointAddress(new Uri("net.tcp://localhost:2202/PatientService"));
            //NetTcpBinding binding = new NetTcpBinding();
            ChannelFactory<IWorldService> factory = new ChannelFactory<IWorldService>("HttpWorldEndpoint");
            var service = factory.CreateChannel();
            Console.WriteLine (service.HelloWorld());

            /*
            Uri baseAddress = new Uri("http://localhost:8888/GettingStarted/");
            ServiceHost selfHost = new ServiceHost(typeof(voxelworld.WorldServiceImpl), baseAddress);
            
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
            */
        }
    }
}
