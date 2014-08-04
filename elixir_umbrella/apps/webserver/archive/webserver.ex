defmodule WebServer do
  use Application

  def start(_type, _args) do
    IO.puts "Starting App..."
    WebServer.Supervisor.start_link
  end
end
