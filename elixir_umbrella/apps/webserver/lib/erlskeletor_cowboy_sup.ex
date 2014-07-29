defmodule Erlskeletor_cowboy_sup do
use Supervisor

# admin api
def start_link() do
    IO.puts "Supervisor.start_link"
    :supervisor.start_link(__MODULE__,[])
end

# behaviour callbacks
def init([]) do
    IO.puts "Supervisor.init"

    #{:ok, { {:one_for_one, 5, 10},
    #       [ 
    #         # {ChildId, StartFunc, Restart, Shutdown, Type, Modules}
    #         {:Erlskeletor_cowboy_http, {:Erlskeletor_cowboy_sup, :start_listeners, []}, :permanent, 1000, :worker, [:Erlskeletor_cowboy_sup]}
    #       ]}
    #}

    children = [
      worker(Erlskeletor_cowboy_worker, [], [restart: :permanent, shutdown: 1000])
    ]

    Supervisor.start_link(children, :one_for_one)

end


end
