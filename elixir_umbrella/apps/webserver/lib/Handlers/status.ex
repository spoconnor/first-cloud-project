defmodule Handlers.Status do
  @behaviour :cowboy_http_handler

  #def init({_any, :http}, req, []) do
  def init(_Type, req, []) do
    {:ok, req, :undefined}
  end

  def handle(req, state) do
    IO.puts "Handlers.Status called!"
    #{:ok, data} = File.read "assets/index.html"
    #{:ok, req2} = :cowboy_req.reply 200, [], data, req
    {:ok, req2} = :cowboy_req.reply 200,[{"content-type", "text/plain"}], "Hello World!", req
    #{:ok, req2} = :cowboy_req.reply 200,[], "Hello World!", req
    {:ok, req2, state}
  end

  def terminate(_reason, _request, _state) do
    :ok
  end
end

