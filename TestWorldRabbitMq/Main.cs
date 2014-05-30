using System;
using System.ServiceModel;
using System.ServiceModel.Channels;
using RabbitMQ.ServiceModel;
using System.Threading;
using voxelworld;

namespace TestWorldRabbitMq
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            Console.WriteLine ("Hello World!");

            m_client = GetClient(GetBinding());
            m_client.Log(new LogData(LogLevel.High, "Hello Rabbit"));
            m_client.Log(new LogData(LogLevel.Medium, "Hello Rabbit"));
            m_client.Log(new LogData(LogLevel.Low, "Hello Rabbit"));
            m_client.Log(new LogData(LogLevel.Low, "Last Message"));


            StopClient(m_client);
        }
 
        private static ChannelFactory<IRabbitMqService> m_factory;
        private static IRabbitMqService m_client;
        
 
        public static Binding GetBinding() {
            //return new WSHttpBinding();
			return new RabbitMQBinding(System.Configuration.ConfigurationManager.AppSettings["manual-test-broker-hostname"],
                int.Parse(System.Configuration.ConfigurationManager.AppSettings["manual-test-broker-port"]),
                RabbitMQ.Client.Protocols.DefaultProtocol);
        }
        
        public static IRabbitMqService GetClient(Binding binding)
        {
            ((RabbitMQBinding)binding).OneWayOnly = true;
            m_factory = new ChannelFactory<IRabbitMqService>(binding, "soap.amqp:///LogService");
            //factory = new ChannelFactory<IRabbitMqService>(binding, "http://localhost/LogService");
            m_factory.Open();
            return m_factory.CreateChannel();
        }
        
        public static void StopClient(IRabbitMqService client)
        {
            Console.Write("  Stopping Client...");
            
            ((IClientChannel)client).Close();
            m_factory.Close();
            
            Console.WriteLine("[DONE]");
        }

    }

}
