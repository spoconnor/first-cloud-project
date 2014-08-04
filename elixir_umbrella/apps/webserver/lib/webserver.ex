defmodule Webserver do
use Application

@webworker Webserver.Worker

def start(_startType, _startArgs) do
  IO.puts "Ensure all started"
  :application.ensure_all_started(Webserver)

  IO.puts "Starting cowboy..."

  port = Application.get_env(:Webserver, :http_port)
  listenerCount = Application.get_env(:Webserver, :http_listener_count)
  IO.puts("Listening on port #{port}")

  dispatch =
    :cowboy_router.compile([
       {
         :_,
         [
            {"/", Webserver.Toppage.Handler, []},
            #{"/", :cowboy_static, {:file, "index.html"}},
            {"/events", Webserver.Events.Handler, []},
            {"/foobar", Webserver.Foobar.Handler, []}
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
    
  {:ok, _} = :cowboy.start_http(:Welserver.Http, listenerCount, ranchOptions, cowboyOptions)

  {:ok, _pid} = Webserver.Supervisor.start_link

end

end


