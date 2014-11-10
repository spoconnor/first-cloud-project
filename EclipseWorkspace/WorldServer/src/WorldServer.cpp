#include <SimpleAmqpClient.h>

#include <iostream>
#include <stdlib.h>

using namespace AmqpClient;
int main()
{
    const std::string IN_EXCHANGE_NAME = "WorldServer_In";
    const std::string OUT_EXCHANGE_NAME = "WorldServer_Out";
    const std::string ROUTING_KEY = "RoutingKey";
    const std::string CONSUMER_TAG = "ConsumerTag";
    const std::string QUEUE_NAME = "Notify_Queue";

    try
    {
        Channel::ptr_t channelIn = Channel::Create();

        channelIn->DeclareExchange(IN_EXCHANGE_NAME, Channel::EXCHANGE_TYPE_FANOUT);
        std::string queueIn = channelIn->DeclareQueue(QUEUE_NAME);
        channelIn->BindQueue(queueIn, IN_EXCHANGE_NAME, ROUTING_KEY);
        channelIn->BasicConsume(queueIn, CONSUMER_TAG);

        Channel::ptr_t channelOut = Channel::Create();
        channelOut->DeclareExchange(OUT_EXCHANGE_NAME, Channel::EXCHANGE_TYPE_FANOUT);
        std::string queueOut = channelOut->DeclareQueue(QUEUE_NAME);
        channelOut->BindQueue(queueOut, OUT_EXCHANGE_NAME, ROUTING_KEY);
        channelOut->BasicConsume(queueOut, CONSUMER_TAG);

        Envelope::ptr_t env;
        for (int i = 0; i < 3; ++i)
        {
            if (channelIn->BasicConsumeMessage(CONSUMER_TAG, env, 0))
            {
                std::cout << "Envelope received: \n"
                          << " Exchange: " << env->Exchange()
                          << "\n Routing key: " << env->RoutingKey()
                          << "\n Consumer tag: " << env->ConsumerTag()
                          << "\n Delivery tag: " << env->DeliveryTag()
                          << "\n Redelivered: " << env->Redelivered()
                          << "\n Body: " << env->Message()->Body() << std::endl;

                channelOut->BasicPublish(OUT_EXCHANGE_NAME, ROUTING_KEY, env->Message());
            }
            else
            {
                std::cout << "Basic Consume failed.\n";
            }
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

