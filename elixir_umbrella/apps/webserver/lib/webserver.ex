defmodule Webserver do
use Application

@webworker Webserver.Worker

def start(_startType, _startArgs) do
  IO.puts "Ensure all started"
  :application.ensure_all_started(Webserver)

  {:ok, _pid} = Webserver.Supervisor.start_link
end

end


