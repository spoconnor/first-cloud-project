using System;
using System.ServiceModel;
using System.ServiceModel.Description;
using Sean.World;
using System.Configuration;
using System.ServiceModel.Channels;
using System.Threading;

namespace Sean.World
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            Console.WriteLine ("Hello World!");

            //StartRabbitMqService(GetBinding());

            //ServiceHost rabbitHost = new ServiceHost(typeof(RabbitMqService));
			//
            //ServiceHost rabbitHost = new ServiceHost(typeof(RabbitMqService),
            //                                         new Uri("soap.amqp:///"));
            //rabbitHost.AddServiceEndpoint(typeof(IRabbitMqService),
            //                              new RabbitMQBinding("localhost", 5672, "guest", "guest", "/", 8192, 
            //                              RabbitMQ.Client.Protocols.AMQP_0_9_1),
            //                              "Rabbit");

            ServiceHost selfHost = new ServiceHost(typeof(Sean.World.RestApi.WorldServiceImpl));

            try
            {
                selfHost.Open();
				//rabbitHost.Open();
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
