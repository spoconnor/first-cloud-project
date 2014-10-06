defmodule Webserver.Worker do
use GenServer

# Client API

 def start_link(opts \\ []) do
  IO.puts "Starting worker link..."
  GenServer.start_link(__MODULE__, :ok, opts)
 end

 def stop() do
  GenServer.call(:stop)
 end

# Server Callbacks

 def init(:ok) do
  IO.puts "Starting cowboy worker..."

  port = Application.get_env(:Webserver, :http_port)
  listenerCount = Application.get_env(:Webserver, :http_listener_count)
  IO.puts("Listening on port #{port}")

  dispatch =
    :cowboy_router.compile([
       {
         :_,
         [
            {"/events", Webserver.Events.Handler, []},
            {"/foobar", Webserver.Foobar.Handler, []},
            {"/api", Webserver.RestApi.Handler, []},
            {"/ws", :cowboy_static, {:priv_file, :Webserver, "ws_index.html"}},
            {"/websocket", Webserver.Websocket.Handler, []},
            {"/static/[...]", :cowboy_static, {:priv_dir, :Webserver, "static"}},
            #{"/api/[:id]", [{:v1, :int}], Webserver.Toppage.Handler, []},
            {"/[...]", :cowboy_static, {:file, "priv/index.html"}},
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

  {:ok, {}}
 end

 def handle_call(:stop, _from, state) do
  {:stop, :normal, :ok, state}
 end


end


