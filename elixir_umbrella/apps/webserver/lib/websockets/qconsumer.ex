defmodule Websocket.QConsumer do
  use GenServer
  use AMQP

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  @exchange      "webserver_exchange"
  @queue         "notify"
  @queue_error   "#{@queue}_error"

  def init(:ok) do
    {:ok, conn} = Connection.open("amqp://guest:guest@localhost")
    {:ok, chan} = Channel.open(conn)
    # Limit unacknowledged messages to 10
    Basic.qos(chan, prefetch_count: 10)
    Queue.declare(chan, @queue_error, durable: false)
    # Messages that cannot be delivered to any consumer in 
    # the main queue will be routed to the error queue
    Queue.declare(chan, @queue, durable: false, arguments: [
      {"x-dead-letter-exchange", :longstr, ""}, 
      {"x-dead-letter-routing-key", :longstr, @queue_error}])
    Exchange.fanout(chan, @exchange, durable: true)
    Queue.bind(chan, @queue, @exchange)
    # Register the GenServer process as a consumer
    Basic.consume(chan, @queue)
    {:ok, chan}
  end

  def handle_info({payload, %{delivery_tag: tag, redelivered: redelivered}}, chan) do
    spawn fn -> consume(chan, tag, redelivered, payload) end
    {:noreply, chan}
  end

  defp consume(channel, tag, redelivered, payload) do
    try do
      IO.puts("Received msg from queue: #{payload}")
      Websocket.EsWebsock.notify(Websocket.Worker, payload)
      Basic.ack channel, tag
      #Basic.reject channel, tag, requeue: false
    rescue
      exception ->
        # Requeue unless it's a redelivered message.
        # This means we will retry consuming a message once in case of exception
        # before we give up and have it moved to the error queue
        Basic.reject channel, tag, requeue: not redelivered
        IO.puts "Error reading '#{payload}'"
        IO.puts Exception.message(exception)
    end
  end

end

