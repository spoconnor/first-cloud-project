defmodule Webserver.Websocket.Handler do
@behaviour :cowboy_websocket_handler

def init(_transport, req, []) do
  IO.puts("Websocket Handler init")
  {:cowboy_websocket, req, []}
end

def websocket_handle({:text, msg}, req, state) do
  IO.puts("Websocket Handle received text")
  {:reply, {:text, << "That's what she said! ", msg >>}, req, state}
end

def websocket_handle(_data, req, state) do
  IO.puts("Websocket Handle received data")
  {:ok, req, state}
end

def websocket_info({:timeout, _ref, msg}, req, state) do
  IO.puts("Websocket info")
  :erlang.start_timer(1000, self(), <<"How' you doin'?">>)
  {:reply, {:text, msg}, req, state}
end

def websocket_info(_info, req, state) do
  IO.puts("Websocket info")
  {:ok, req, state}
end

end
