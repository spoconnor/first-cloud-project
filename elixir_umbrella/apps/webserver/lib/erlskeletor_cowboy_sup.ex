defmodule Erlskeletor_cowboy_sup do
use Supervisor

# admin api
def start_link do
    IO.puts "Supervisor.start_link"
    Supervisor.start_link(__MODULE__,:ok)
end

@worker Erlskeletor_cowboy_worker

# behaviour callbacks
def init(:ok) do
    IO.puts "Supervisor.init"

    #{:ok, { {:one_for_one, 5, 10},
    #       [ 
    #         # {ChildId, StartFunc, Restart, Shutdown, Type, Modules}
    #         {:Erlskeletor_cowboy_http, {:Erlskeletor_cowboy_sup, :start_listeners, []}, :permanent, 1000, :worker, [:Erlskeletor_cowboy_sup]}
    #       ]}
    #}

    children = [
      worker(Erlskeletor_cowboy_worker, [[name: @worker]]) 
      #, [restart: :permanent, shutdown: 1000])
    ]

    supervise(children, strategy: :one_for_one)

end


end
