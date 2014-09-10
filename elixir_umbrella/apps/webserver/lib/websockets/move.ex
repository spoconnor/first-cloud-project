defmodule Websocket.Move do

def move(simple{id=ID,map=Map},X,Y,State = state{maps=Maps})  do
  MapDict=array:get(Map,Maps),
  Now=u:munixtime(),
  case dict:find(ID,MapDict) do
    {ok, Record=%User{lastAction=LastAction,user=User}} when (Now-LastAction)>349 ->
      NewMaps=array:set(Map,dict:store(ID,%User{lastAction=Now,x=X,y=Y},MapDict),Maps),
      es_websock:sendToAll(MapDict,ID,["move @@@ ",User,"||",X,"||",Y]),
      {noreply,State state{maps=NewMaps}};
    _ -> {noreply,State}
  end
end

end

    
