defmodule Rabbit.Producer do
  use Behaviour

  defmodule RabbitProducerState do
    defstruct( amqp: nil, channel: nil, config: nil)
  end

  def start_link(config) do
    :gen_server.start_link({ :local, :producer }, __MODULE__, config, [])
  end

  def init(config) do
    amqp = Exrabbit.Utils.connect(host: "localhost",
                                  port: 5672,
                                  username: "guest",
                                  password: "guest")

    channel = Exrabbit.Utils.channel amqp
    queue = Exrabbit.Utils.declare_queue(channel, config.from_device, auto_delete: false, durable: true)

    {:ok, %RabbitProducerState{amqp: amqp, channel: channel, config: config} }
  end

  def send(connection_id, message, from_device_seq) do
    :gen_server.cast(:producer, {:send, connection_id, message, from_device_seq})
  end

  def handle_cast({:send, connection_id, message, from_device_seq}, state ) do
    props = [headers: [{"connection_id", :longstr, connection_id}, {state.config.from_device_seq, :long, from_device_seq}]]
    Exrabbit.Utils.publish(state.channel, "", state.config.from_device, message, props)
    {:noreply, state }
  end

  def terminate(reason, state) do
    IO.puts "Rabbit producer terminating. Reason was:"
    IO.inspect reason
    Exrabbit.Utils.channel_close(state.channel)
    Exrabbit.Utils.disconnect(state.amqp)
    :ok
  end
end
