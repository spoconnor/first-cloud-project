defmodule WebServer do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Define workers and child supervisors to be supervised
      # worker(WebServer.Worker, [arg1, arg2, arg3])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: WebServer.Supervisor]
    Supervisor.start_link(children, opts)
  
    # My code...
    #dispatch = [{:_, [
    #  {[], Handlers.Status, []},
    #]}]
    dispatch = :cowboy_router.compile([
      {:_, [{"/css/[...]", :cowboy_static, [
          {:directory, {:priv_dir, :testElixirWithCowboy, [<<"public/css">>]}},
          {:mimetypes, [{<<".css">>, [<<"text/css">>]}]}
        ]},
            
        {"/js/[...]", :cowboy_static, [
          {:directory, {:priv_dir, :testElixirWithCowboy, [<<"public/js">>]}},
          {:mimetypes, [{<<".js">>, [<<"application/javascript">>]}]}
        ]},
 
        {"/[...]", :cowboy_static, [
          {:directory, {:priv_dir, :testElixirWithCowboy, [<<"public/">>]}},
          {:mimetypes, [{<<".html">>, [<<"text/html">>]}]},
        ]}
      ]}
    ])

    {:ok, _} = :cowboy.start_http(:http, 100, [port: 8080], [env: [dipatch: dispatch]])

    #:cowboy.start_listener :example_http_listener, 100,
    #:cowboy_tcp_transport, [{:port, 8080}],
    #:cowboy_http_protocol, [{:dispatch, dispatch}]

    #ExampleSup.start_link
  end

  def main(_) do
    start()
    IO.gets("Press any key")
  end

  def start do
    start_app :lager
    start_app :cowboy
    start_app :example
    start :undefined, []
  end

  def stop(_state) do
    stop_app :example
    stop_app :cowboy
    stop_app :lager
    :ok
  end

  def exit do
    stop(:ok)
    System.halt(0)
  end

  defp start_app(app) do
    _status = :application.start app
  end

  defp stop_app(app) do
    _status = :application.stop app
  end

end
