defmodule Websocket.Logout do

def logout(simple{id=ID,map=Map},State = state{maps=Maps,lookupByID=LBID,lookupByName=LBName,lookupByIP=LBIP}) do
  MapDict=array:get(Map,Maps)
  case dict:find(ID,MapDict) do
    {ok, %User{user=User,pid=Pid,ip=IP}} ->
        send Pid, {die,"Disconnected"}
        es_websock:sendToAll(MapDict,ID,["logout @@@ ",User]),
        LBID1=dict:erase(ID,LBID)
        LBIP1=gb_trees:delete_any(IP,LBIP)
        LBName1=gb_trees:delete_any(User,LBName)
        {noreply,State state{maps=array:set(Map,dict:erase(ID,MapDict),Maps),lookupByID=LBID1,lookupByName=LBName1,lookupByIP=LBIP1}};
    _ -> {noreply,State}
  end
end

end
