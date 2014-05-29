using System;
using System.ServiceModel;
using System.ServiceModel.Description;
using voxelworld;
using System.Configuration;
using System.ServiceModel.Channels;
using RabbitMQ.ServiceModel;
using System.Threading;

namespace voxelworld
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            Console.WriteLine ("Hello World!");

            StartRabbitMqService(GetBinding());



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

            StopRabbitMqService();

        }


        public static Binding GetBinding() {
            //return new WSHttpBinding();
            
            return new RabbitMQBinding(System.Configuration.ConfigurationManager.AppSettings["manual-test-broker-hostname"],
                                       int.Parse(System.Configuration.ConfigurationManager.AppSettings["manual-test-broker-port"]),
                                       RabbitMQ.Client.Protocols.FromConfiguration("manual-test-broker-protocol"));
        }


        public static void StartRabbitMqService(Binding binding)
        {
            Console.Write("  Binding RabbitMq Service...");
            m_host = new ServiceHost(typeof(RabbitMqService), new Uri("soap.amqp:///"));
            ((RabbitMQBinding)binding).OneWayOnly = true;
            //host = new ServiceHost(typeof(LogService), new Uri("http://localhost/"));
            
            m_host.AddServiceEndpoint(typeof(IRabbitMqService), binding, "RabbitMqService");
            m_host.Open();
            m_serviceStarted = true;
            
            Thread.Sleep(500);
            Console.WriteLine("[DONE]");
        }

        public static void StopRabbitMqService()
        {
            Console.Write("  Stopping RabbitMq Service...");
            if (m_serviceStarted)
            {
                m_host.Close();
                m_serviceStarted = false;
            }
            
            Console.WriteLine("[DONE]");
        }
        
        private static ServiceHost m_host;
        private static bool m_serviceStarted;

    }
}
