defmodule Toppage_Handler do

  def init(_Type, Req, []) do
    {:ok, Req, :undefined}
  end

  def handle(Req, State) do
    {:ok, Req2} = :cowboy_req.reply(200, [
      {"content-type", "text/plain"}
    ], "Hello world!", Req)
    {:ok, Req2, State}
  end

  def terminate(_Reason, _Req, _State) do
    :ok
  end

end
