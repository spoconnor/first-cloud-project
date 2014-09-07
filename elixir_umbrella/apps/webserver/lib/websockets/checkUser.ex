defmodule checkUser do

def checkUser(Record = #user{ip=IP}, State = #state{banned=Banned}) do
  case lists:any(fun(IP1) when IP1=:=IP -> true;(_)->false end,Banned) of
    false -> checkUser1(IP,Record,State);
    true -> u:trace("fail at IP ban"), {noreply,State}
  end
end

def checkUser1(IP,Record=#user{ip=IP},State=#state{lookupByIP=LBIP}) do
  case gb_trees:lookup(IP,LBIP) of
    none -> checkUser2(IP,Record,State);
    {value,_} ->  u:trace("fail at already logged in"), {reply,fail,State}
  end
end

def checkUser2(IP,Record,State) do
  #user{user=User,x=X,y=Y,sprite=Sprite,sock=Sock,auth=Auth} = Record
  #state{maps=Maps,increment=ID,lookupByID=LBID,lookupByName=LBName,lookupByIP=LBIP} = State
  Map0Dict=array:get(0,Maps)

  %%send all user locations for current map
  Gather =
    fun(_,#user{x=X1,y=Y1,sprite=Sprite1,auth=Auth1,user=User1},Acc)->
       [[",[\"",User1,"\",\"",Sprite1,"\",\"",Auth1,"\",\"",X1,"\",\"",Y1,"\"]"]|Acc]
    end
  case lists:flatten(dict:fold(Gather,[],Map0Dict)) of
    [_|Out] -> void
    [] -> Out="[]"
  end
  websockets:msg(Sock,"all",["[",Out,"]"])

  NewDict=dict:store(ID,Record#user{id=ID},Map0Dict)
  Maps1=array:set(0,NewDict,Maps)
  LBID1=dict:store(ID,0,LBID)
  LBIP1=gb_trees:enter(IP,{0,ID},LBIP)
  LBName1=gb_trees:enter(User,{0,ID},LBName)
  array:foldl(fun(_,Dict,_) -> es_websock:sendToAll(Dict,ID,["login @@@ ",User,"||",Sprite,"||",Auth,"||",X,"||",Y]) end,0,Maps)
  {reply,ID,State#state{maps=Maps1,increment=ID+1,lookupByID=LBID1,lookupByIP=LBIP1,lookupByName=LBName1}}
end
