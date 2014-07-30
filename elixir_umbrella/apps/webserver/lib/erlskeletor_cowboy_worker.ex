defmodule Erlskeletor_cowboy_worker do
use GenServer

def start_link(state) do
  IO.puts "Starting worker link..."
  GenServer.start_link(__MODULE__, state, [name: :Erlskeletor_cowboy_workder])
end

def init(_state) do
  IO.puts "Starting worker..."
  port = Application.get_env(:Erlskeletor_cowboy, :http_port)
  listenerCount = Application.get_env(:Erlskeletor_cowboy, :http_listener_count)
  IO.puts("Listening on port #{port}")

  dispatch =
    :cowboy_router.compile([
       {
         :_,
         [
            {"/", :cowboy_static, {:file, "index.html"}},
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

#def stop(_state) do
#  :ok
#end


end
