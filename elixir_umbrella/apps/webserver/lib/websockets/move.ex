defmodule Websocket.Move do

def move(%Simple{id: id, map: map},x,y,state = %State{maps: maps})  do
  mapDict=:array.get(map,maps)
  now=Lib.munixtime()
  case :dict.find(id,mapDict) do
    {:ok, record=%User{lastAction: lastAction, user: user}} when (now-lastAction)>349 ->
      newMaps=:array.set(map,:dict.store(id,%User{lastAction: now, x: x, y: y},mapDict),maps)
      EsWebsock.sendToAll(mapDict,id,["move @@@ ",user,"||",x,"||",y])
      {noreply,%State{maps: newMaps}}
    _ -> {noreply,state}
  end
end

end
