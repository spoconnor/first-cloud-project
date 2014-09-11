defmodule Websocket.Logout do

def logout(%Simple{id: id, map: map},state = %State{maps: maps, lookupByID: lbid, lookupByName: lbName, lookupByIP: lbip}) do
  mapDict=:array.get(map,maps)
  case :dict.find(id,mapDict) do
    {ok, %User{user: user, pid: pid, ip: ip}} ->
        send pid, {die,"Disconnected"}
        es_websock.sendToAll(mapDict,id,["logout @@@ ",user])
        lbid1=:dict.erase(id,lbid)
        lbip1=:gb_trees.delete_any(ip,lbip)
        lbName1=:gb_trees.delete_any(user,lbName)
        {noreply, %State{maps: :array.set(map, :dict.erase(id,mapDict),maps), lookupByID: lbid1, lookupByName: lbName1, lookupByIP: lbip1}}
    _ -> {noreply,state}
  end
end

end
