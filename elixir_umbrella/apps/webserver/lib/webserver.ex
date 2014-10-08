defmodule Webserver do
use Application

@webworker Webserver.Worker

def start(_startType, _startArgs) do
  IO.puts "Ensure all started"

msg = Messages.Coords.new(x: 1, y: 2)
encoded = Messages.Coords.encode(msg)
IO.puts(encoded)

  :application.ensure_all_started(Webserver)

  {:ok, _pid} = Webserver.Supervisor.start_link

#  IO.puts "Configuring Riak..."
#  {:ok, _pid} = Riak.start
#  {:ok, _pid} = Riak.configure([host: '127.0.0.1', port: 8087])

end

def stop(_state) do
  :ok
end

end


