defmodule Handlers.Status do
  @behaviour :cowboy_http_handler

  def init({_any, :http}, req, []) do
    {:ok, req, :undefined}
  end

  def handle(req, state) do
    IO.puts "Handlers.Status called!"
    #{:ok, data} = File.read "assets/index.html"
    #{:ok, req} = :cowboy_http_req.reply 200, [], data, req
    {:ok, req} = :cowboy_http_req.reply 200,[{"content-type", "text/plain"}], "Hello World!", req
    {:ok, req, state}
  end

  def terminate(_request, _state) do
    :ok
  end
end

