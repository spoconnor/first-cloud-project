defmodule Erlskeletor_cowboy do
use Application

@webworker Erlskeletor_cowboy_worker

def start(_startType, _startArgs) do
  IO.puts "Ensure all started"
  :application.ensure_all_started(Erlskeletor_cowboy)

  IO.puts "Starting cowboy..."

  port = Application.get_env(:Erlskeletor_cowboy, :http_port)
  listenerCount = Application.get_env(:Erlskeletor_cowboy, :http_listener_count)
  IO.puts("Listening on port #{port}")

  dispatch =
    :cowboy_router.compile([
       {
         :_,
         [
            {"/", Erlskeletor_toppage_handler, []},
            #{"/", :cowboy_static, {:file, "index.html"}},
            {"/events", Erlskeletor_cowboy_events_handler, []},
            {"/foobar", Erlskeletor_cowboy_foobar_handler, []}
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
    
  {:ok, _} = :cowboy.start_http(:Erlskeletor_cowboy_http, listenerCount, ranchOptions, cowboyOptions)

  {:ok, _pid} = Erlskeletor_cowboy_sup.start_link

  IO.gets "Press enter"
  :ok
end

end


