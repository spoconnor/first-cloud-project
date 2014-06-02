using System;
using System.ServiceModel;
using Sean.World.RestApi;
using System.Net;

namespace Sean.World.TestRest
{
    class MainClass
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");

           // string targetUri = "http://localhost:8888/WorldService/World/hello";
           // HttpWebRequest httpRequest = (HttpWebRequest)WebRequest.Create(targetUri);

            //EndpointAddress address = new EndpointAddress(new Uri("net.tcp://localhost:2202/PatientService"));
            //NetTcpBinding binding = new NetTcpBinding();
            ChannelFactory<IWorldService> factory = new ChannelFactory<IWorldService>("HttpWorldEndpoint");
            var service = factory.CreateChannel();
            Console.WriteLine (service.HelloWorld("Hey!"));
            Console.WriteLine (service.GetChunkAddress(1,2,3));
            Console.WriteLine (service.SetBlock(1,2,3, 10));

            /*
            Uri baseAddress = new Uri("http://localhost:8888/GettingStarted/");
            ServiceHost selfHost = new ServiceHost(typeof(Sean.World.WorldServiceImpl), baseAddress);
            
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
