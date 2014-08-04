defmodule WebServer.Worker do
  @Behaviour

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

  def start(_type, _args) do
    deps_started
    IO.puts "Starting worker..."

    :supervisor.start_link {:local, __MODULE__}, __MODULE__, []

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

    WebServer.Supervisor.start_link

  end

  def stop(_state) do
    #stop_app :example
    #stop_app :cowboy
    #stop_app :lager
    :ok
  end

  #def exit do
    #stop(:ok)
    #System.halt(0)
  #end

end
