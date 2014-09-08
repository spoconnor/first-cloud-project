defmodule Websocket.Say do

define(FLOOD, 2000)

def say(simple{id=ID,map=Map},Message,State = state{maps=Maps}) do
  MapDict=array:get(Map,Maps),
  say1({ID,Map,MapDict,Message,Maps},dict:find(ID,MapDict),State).

def say1({ID,Map,MapDict,Message,Maps},{ok, Record},State) do
  user{lastMessage=LastMessage,floodTest=[_|FloodTest],user=User,sock=Sock} = Record,
  Unix=u:munixtime(),
  Waited=Unix-LastMessage,
  Difference=Waited-?FLOOD,
  case Waited > ?FLOOD do 
    true ->
      es_websock:sendToAll(MapDict,ID,["say @@@ ",User,"||",Message]);
    false ->
      websockets:alert(Sock,["Error: Flooding, message not sent, wait ",?FLOOD - Difference," more seconds."])
  end,
  FloodTest1=FloodTest ++ [Difference],
  {Len,Sum} = lists:foldl(fun(X,{Len,Sum}) -> {Len+1,Sum+X} end,{0,0},FloodTest1),
  Test = Sum div Len,
%  u:trace("Flood Test",Test),
  case Test<1000 do
    false ->
      NewDict=dict:store(ID,Record user{floodTest=FloodTest1,lastMessage=Unix,lastAction=Unix},MapDict),
      NewMap=array:set(Map,NewDict,Maps),
      {noreply,State state{maps=NewMap}};
    true ->
      state{banned=Banned} = State,
      user{ip=IP} = Record,
      NewState=State state{banned=[IP|Banned]},
      kill:kill(NewState,ID,"Excess Flooding, you have been banned")
  end;
def say1(_,_,State) do
  State
end

end
    
