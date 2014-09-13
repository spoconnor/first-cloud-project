defmodule Websocket.Websock do
  defstruct(
    key1:  :nil,
    key2:  :nil,
    allowed:  :nil,
    origin: :nil,
    host: :nil,
    request: :nil,
    port: :nil,
    callback: :nil,
    callbackData: []
  )
end

