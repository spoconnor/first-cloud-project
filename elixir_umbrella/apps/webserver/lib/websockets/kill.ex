defmodule Websocket.Kill do

def kill(State,ID,Message) do
  state{maps=Maps,banned=IPBlock,lookupByID=LBID,lookupByName=LBName,lookupByIP=LBIP} = State
  {ok,Map}=dict:find(ID,LBID)
  MapDict=array:get(Map,Maps)
  {ok, user{ip=IP,user=Username,pid=Pid,sock=Sock}} = dict:find(ID,MapDict)
  websockets:alert(Sock,Message)
  Pid ! {kill,Message}
  array:foldl(fun(_,Dict,_) -> es_websock:sendToAll(Dict,ID,["logout @@@ ",Username]),0 end,0,Maps)
  Maps1=array:set(Map,dict:erase(ID,MapDict),Maps)
  LBID1=dict:erase(ID,LBID)
  LBName1=removeID(ID,LBName)
  LBIP1=removeID(ID,LBIP)
  IPBlock1=lists:delete(IP,IPBlock)
  {noreply,State state{maps=Maps1,banned=IPBlock1,lookupByID=LBID1,lookupByName=LBName1,lookupByIP=LBIP1}}
end
   
def removeID(ID,GB) do
  Filter=fun({_,ID1}) when ID1===ID -> false;(_) -> true end,
  gb_trees:from_orddict(lists:filter(Filter,gb_trees:to_list(GB)))
end

end
    
