defmodule Erlskeletor_cowboy_sup do
@behaviour :supervisor

# admin api
def start_link() do
    :supervisor.start_link {:local, __MODULE__}, __MODULE__, []
end

def start_listeners() do
    port = Application.get_env(:Erlskeletor_cowboy, :http_port)
    listenerCount = Application.get_env(:Erlskeletor_cowboy, :http_listener_count)
    
    dispatch =
        :cowboy_router.compile([
                               {
                                 '_',
                                 [
                                  {"/", :cowboy_static, {:file, "src/index.html"}},
                                  {"/events", :Erlskeletor_cowboy_events_handler, []},
                                  {"/foobar", :Erlskeletor_cowboy_foobar_handler, []}
                                 ]
                               }
                              ])
    
    ranchOptions =
        [ 
          {:port, port}
        ]
    cowboyOptions =
        [ 
          {:env, [
                 {:dispatch, dispatch}
                ]},
          {:compress,  true},
          {:timeout,   12000}
        ]
    
    :cowboy.start_http(:Erlskeletor_cowboy_http, listenerCount, ranchOptions, cowboyOptions)
end

# behaviour callbacks
def init(_) do
    {:ok, { {:one_for_one, 5, 10},
           [ 
             # {ChildId, StartFunc, Restart, Shutdown, Type, Modules}
             {Erlskeletor_cowboy_http, {:Erlskeletor_cowboy_sup, start_listeners, []}, :permanent, 1000, :worker, [:Erlskeletor_cowboy_sup]}
           ]}
    }
end


end
