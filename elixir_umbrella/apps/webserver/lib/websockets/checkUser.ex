defmodule Websocket.CheckUser do

def checkUser(record = %User{ip: ip}, state = %State{banned: banned}) do
  case :lists.any(fn(IP1) -> IP1===IP end, banned) do
    :false -> checkUser1(ip,record,state)
    :true -> u.trace("fail at IP ban") 
  end
  {noreply,state}
end

def checkUser1(ip,record = %User{ip: ip}, state = %State{lookupByIP: lbip}) do
  case :gb_trees.lookup(ip,lbip) do
    none -> checkUser2(ip,record,state);
    {value,_} ->  u.trace("fail at already logged in")
  end
  {reply,fail,state}
end

def checkUser2(ip,record,state) do
  %User{user: user, x: x, y: y, sprite: sprite, sock: sock, auth: auth} = record
  %State{maps: maps, increment: id, lookupByID: lbid, lookupByName: lbName, lookupByIP: lbip} = state
  map0Dict=:array.get(0,maps)

  # send all user locations for current map
  gather =
    fn(_, %User{x: x1, y: y1, sprite: sprite1, auth: auth1, user: user1}, acc)->
       [[",[\"",user1,"\",\"",sprite1,"\",\"",auth1,"\",\"",x1,"\",\"",y1,"\"]"]|acc]
    end
  case :lists.flatten(:dict.fold(gather,[],map0Dict)) do
    [_|out] -> void
    [] -> out="[]"
  end
  :websockets.msg(sock,"all",["[",out,"]"])

  newDict=:dict.store(id, %User{id: id},map0Dict)
  maps1=:array.set(0,newDict,maps)
  lbid1=:dict.store(id,0,lbid)
  lbip1=:gb_trees.enter(ip,{0,id},lbip)
  lbName1=:gb_trees.enter(user,{0,id},lbName)
  :array.foldl(fun(_,dict,_) do EsWebsock.sendToAll(dict,id,["login @@@ ",user,"||",sprite,"||",auth,"||",x,"||",y]) end,0,maps)
  {reply,id,state state{maps=maps1,increment=id+1,lookupByID=lbid1,lookupByIP=lbip1,lookupByName=lbName1}}
end

end
