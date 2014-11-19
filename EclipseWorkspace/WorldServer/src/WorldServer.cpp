#include <SimpleAmqpClient.h>

#include <iostream>
#include <stdlib.h>

using namespace AmqpClient;


#ifdef _WIN32
    #include <windows.h>

    void msleep(unsigned milliseconds)
    {
        Sleep(milliseconds);
    }
#else
    #include <unistd.h>

    void msleep(unsigned milliseconds)
    {
        usleep(milliseconds * 1000); // takes microseconds
    }
#endif


int main()
{
	const std::string HOSTNAME = "localhost";
	const int PORT = 5672;
	const std::string USERNAME = "guest";
	const std::string PASSWORD = "guest";
	const std::string VHOST = "/";
    const std::string EXCHANGE_NAME = "MyExchange";

    const std::string OUTBOUND_ROUTING_KEY = "Outbound";
    const std::string CONSUMER_TAG = "ConsumerTag";

    const std::string INBOUND_QUEUE_NAME = "InboundQueue";
    const std::string OUTBOUND_QUEUE_NAME = "OutboundQueue";
    //const std::string ERROR_QUEUE_NAME = "ErrorQueue";

    try
    {
    	Channel::ptr_t channel = Channel::Create(HOSTNAME, PORT, USERNAME, PASSWORD, VHOST);
        channel->BasicConsume(INBOUND_QUEUE_NAME, CONSUMER_TAG, true, true, false);

        Envelope::ptr_t env;
        while (true)
        {
            if (channel->BasicConsumeMessage(CONSUMER_TAG, env, 0))
            {
                std::cout << "Envelope received: \n"
                          << " Exchange: " << env->Exchange()
                          << "\n Routing key: " << env->RoutingKey()
                          << "\n Consumer tag: " << env->ConsumerTag()
                          << "\n Delivery tag: " << env->DeliveryTag()
                          << "\n Redelivered: " << env->Redelivered()
                          << "\n Body: " << env->Message()->Body() << std::endl;

                channel->BasicPublish(EXCHANGE_NAME, OUTBOUND_ROUTING_KEY, env->Message());
            }
            else
            {
                std::cout << "Basic Consume failed.\n";
            }
            msleep(5000);
        }

    }
    catch (MessageReturnedException &e)
    {
        std::cout << "Message got returned: " << e.what();
        std::cout << "\nMessage body: " << e.message()->Body();
    }
    catch (AmqpException &e)
    {
        std::cout << "Failure: " << e.what();
    }
}

