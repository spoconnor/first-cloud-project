defmodule Webserver.Supervisor do
use Supervisor

# admin api
def start_link do
    IO.puts "Supervisor.start_link"
    Supervisor.start_link(__MODULE__,:ok)
end

@riakworker Riak
@webworker Webserver.Worker
@es_websock Websocket.Worker
@q_consumer Websocket.QConsumer
@users Websocket.Users

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
      #worker(Riak.Client, []),
      worker(Webserver.Worker, [[name: @webworker]]),
      worker(Websocket.EsWebsock, [[name: @es_websock]]), 
      worker(Websocket.QConsumer, [[name: @q_consumer]]), 
      worker(Websocket.Users, [[name: @users]]), 
      #, [restart: :permanent, shutdown: 1000])
    ]

    supervise(children, strategy: :one_for_one)

end


end
