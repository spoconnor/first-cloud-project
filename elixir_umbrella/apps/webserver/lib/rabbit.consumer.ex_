defmodule Rabbit.Consumer do
  use Exrabbit.Subscriber

  def start_link(config) do
    :gen_server.start_link(Rabbit.Consumer,
                      [ queue: "testQ"],
                      #[host: "localhost",
                      # port: 5672,
                      # username: "guest",
                      # password: "guest",
                      # queue: "testQ"],
                      [])
  end

  def handle_message(msg, state) do
    case parse_message(msg) do
      nil -> IO.puts "Got nil message"
      {tag, payload} ->
        IO.puts "Got message with tag #{tag} and payload #{payload}"
        ack state[:channel], tag
    end
  end

end
