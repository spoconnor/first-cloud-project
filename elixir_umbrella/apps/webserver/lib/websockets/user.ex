defmodule Websocket.User do
  defstruct( 
    x:  "0",
    y:  "0",
    id:  0,
    user:  "",
    map:  0,
    lastMessage:  Websocket.Lib.munixtime()-3000,
    lastAction:  Websocket.Lib.munixtime()-3000,
    floodTest:  [ for x <- 1..6 do 4000 end ], 
    sprite:  "0",
    ip:  :nil,
    auth:  "0",
    sock:  :nil,
    pid:  :nil
  )
end

