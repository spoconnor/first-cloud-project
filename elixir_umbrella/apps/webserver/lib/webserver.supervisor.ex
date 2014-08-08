defmodule Webserver.Supervisor do
use Supervisor

# admin api
def start_link do
    IO.puts "Supervisor.start_link"
    Supervisor.start_link(__MODULE__,:ok)
end

@worker Webserver.Worker

# behaviour callbacks
def init(:ok) do
    IO.puts "Supervisor.init"

    #{:ok, { {:one_for_one, 5, 10},
    #       [ 
    #         # {ChildId, StartFunc, Restart, Shutdown, Type, Modules}
    #         {:Webserver.Http, {:Webserver.Supervisor, :start_listeners, []}, :permanent, 1000, :worker, [:Webserver.Supervisor]}
    #       ]}
    #}

    children = [
      worker(Webserver.Worker, [[name: @worker]]) 
      #, [restart: :permanent, shutdown: 1000])
    ]

    supervise(children, strategy: :one_for_one)

end


end