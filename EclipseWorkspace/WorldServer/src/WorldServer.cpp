#include <SimpleAmqpClient.h>

#include <iostream>
#include <stdlib.h>

using namespace AmqpClient;
int main()
{
    const std::string EXCHANGE_NAME = "SimpleAmqpClientEnvelopeTest";
    const std::string ROUTING_KEY = "SACRoutingKey";
    const std::string CONSUMER_TAG = "SACConsumerTag";

    try
    {
        Channel::ptr_t channel = Channel::Create();

        channel->DeclareExchange(EXCHANGE_NAME, Channel::EXCHANGE_TYPE_FANOUT);
        std::string queue = channel->DeclareQueue("");
        channel->BindQueue(queue, EXCHANGE_NAME, ROUTING_KEY);

        try
        {
            channel->BasicPublish(EXCHANGE_NAME, ROUTING_KEY, BasicMessage::Create("MessageBody"));
            channel->BasicPublish(EXCHANGE_NAME, ROUTING_KEY, BasicMessage::Create("MessageBody2"));
            channel->BasicPublish(EXCHANGE_NAME, ROUTING_KEY, BasicMessage::Create("MessageBody3"));
        }
        catch (MessageReturnedException &e)
        {
            std::cout << "Message got returned: " << e.what();
            std::cout << "\nMessage body: " << e.message()->Body();
            return -1;
        }

        channel->BasicConsume(queue, CONSUMER_TAG);

        Envelope::ptr_t env;
        for (int i = 0; i < 3; ++i)
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
            }
            else
            {
                std::cout << "Basic Consume failed.\n";
            }
        }
    }
    catch (AmqpException &e)
    {
        std::cout << "Failure: " << e.what();
    }
}

