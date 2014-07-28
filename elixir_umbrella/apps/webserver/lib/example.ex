defmodule Example do
  @behaviour :application

  #def start do
  #  start_app :ranch
  #  start_app :lager
  #  start_app :cowboy
  #  start_app :example
  #  start :undefined, []
  #end

  #def stop(_state) do
  #  stop_app :example
  #  stop_app :cowboy
  #  stop_app :lager
  #  :ok
  #end

  #def exit do
  #  stop(:ok)
  #  System.halt(0)
  #end

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
    IO.puts "Starting Example"
    deps_started
    dispatch = :cowboy_router.compile([
     {:_,
     [
#        {"/[...]", :cowboy_static, [
#          {:directory, {:priv_dir, :Handlers.Status, [<<"public/">>]}},
#          {:mimetypes, [{<<".html">>, [<<"text/html">>]}]},
#       ]}

        {"/[...]", :Handlers.Status, []},
     ]}
     # {:directory, {:priv_dir, :Handlers.Status, []}} ]}

    ])
    #dispatch = [{:_, [
    #  {[], Handlers.Status, []},
    #]}]
    #:cowboy.start_listener :example_http_listener, 100,
    #:cowboy_tcp_transport, [{:port, 8080}],
    #:cowboy_http_protocol, [{:dispatch, dispatch}]

    {:ok, _} = :cowboy.start_http(:http, 100, [{:port, 8080}], [{:env, [{:dispatch, dispatch}]}] )

    ExampleSup.start_link 

    IO.gets "Press enter"
    {:ok}
  end

  defp start_app(app) do
    _status = :application.start app
  end

  defp stop_app(app) do
    _status = :application.stop app
  end
end
