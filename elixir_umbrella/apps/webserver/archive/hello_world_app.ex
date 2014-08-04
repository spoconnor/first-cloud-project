defmodule Hello_World_App do
#-behaviour(application).

  def start(_Type, _Args) do
    Dispatch = :cowboy_router.compile([
      {:_, [ {"/", :Toppage_Handler, []} ]}
    ])
    {:ok, _} = :cowboy.start_http(:http, 100, [{:port, 8080}], [
      {:env, [{:dispatch, Dispatch}]}
    ])
    :Hello_World_Sup.start_link()
  end

  def stop(_State) do
    :ok
  end

end
