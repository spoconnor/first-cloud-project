defmodule WebServer.Worker do
  @behaviour :supervisor
  #use Application

  defp deps_started do
    IO.puts "Starting deps..."
    deps = [:ranch, :cowlib, :cowboy]
    Enum.all? deps, &ensure_started/1
  end

  defp ensure_started(app) do
    case :application.start(app) do
      :ok ->
        true
      {:error, {:already_started, _app}} ->
        true
      {:error, {:not_started, dep}} ->
        true = ensure_started(dep)
        ensure_started(app)
      error ->
        IO.puts "Couldnt start #{inspect app}: #{inspect error}"
        error
    end
  end

  def init([]) do
    {:ok, {{:one_for_one, 10, 10}, []}}
  end

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications

  #def start_link do
  #end

  #def start_link(_args) do
  def start_link() do
    deps_started
    IO.puts "Starting worker..."

    :supervisor.start_link {:local, __MODULE__}, __MODULE__, []

    #dispatch = [{:_, [
    #  {[], Handlers.Status, []},
    #]}]

    dispatch = :cowboy_router.compile([
      {:_, [{"/css/[...]", :cowboy_static, [
          {:directory, {:priv_dir, :webServer.Worker, [<<"public/css">>]}},
          {:mimetypes, [{<<".css">>, [<<"text/css">>]}]}
        ]},
         
        {"/js/[...]", :cowboy_static, [
          {:directory, {:priv_dir, :webServer.Worker, [<<"public/js">>]}},
          {:mimetypes, [{<<".js">>, [<<"application/javascript">>]}]}
        ]},
 
        {"/[...]", :cowboy_static, [
          {:directory, {:priv_dir, :webServer.Worker, [<<"public/">>]}},
          {:mimetypes, [{<<".html">>, [<<"text/html">>]}]},
        ]}
      ]}
    ])

    {:ok, _} = :cowboy.start_http(:http, 100, [port: 8080], [env: [dipatch: dispatch]])

    #:cowboy.start_listener :example_http_listener, 100,
    #:cowboy_tcp_transport, [{:port, 8080}],
    #:cowboy_http_protocol, [{:dispatch, dispatch}]

  end

  #def start do
    #start_app :lager
    #start_app :cowboy
    #start_app :example
    #start :undefined, []
  #end

  #def stop(_state) do
    #stop_app :example
    #stop_app :cowboy
    #stop_app :lager
    #:ok
  #end

  #def exit do
    #stop(:ok)
    #System.halt(0)
  #end


end
