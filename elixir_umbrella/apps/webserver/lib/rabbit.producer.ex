defmodule Rabbit.Producer do

defmodule Rabbit.AmqpParamsNetwork do
  @record Record.Extractor.extract(:amqp_params_network, from: Path.join([Mix.Project.deps_path, "amqp_client", "include", "amqp_client.hrl"]))
  defstruct @record

  def record, do: @record
end



#-include("amqp_client.hrl").

#  def test() do
#    # Start a network connection
#    {:ok, connection} = :amqp_connection.start(:amqp_params_network{})
#    # Open a channel on the connection
#    {:ok, channel} = :amqp_connection.open_channel(connection)
#
#    # Declare a queue
#    %'queue.declare_ok'{:queue = Q}
#      = :amqp_channel.call(channel, %'queue.declare'{})
#
#    # Publish a message
#    payload = <<"foobar">>
#    publish = %'basic.publish'{:exchange = <<>>, :routing_key = q}
#    :amqp_channel.cast(channel, publish, %amqp_msg{:payload = payload})
#
#    # Get the message back from the queue
#    Get = %'basic.get'{:queue = Q}
#    {#'basic.get_ok'{:delivery_tag = tag}, content}
#       = :amqp_channel.call(channel, get)
#
#    # Do something with the message payload
#    # (some work here)
#
#    # Ack the message
#    :amqp_channel.cast(channel, %'basic.ack'{delivery_tag = tag})
#
#    # Close the channel
#    :amqp_channel.close(channel)
#    # Close the connection
#    :amqp_connection.close(connection)
#
#    :ok
#  end

end

