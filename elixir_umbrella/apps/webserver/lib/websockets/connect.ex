defmodule Websocket.Connect do

define(TIMEOUT,30*1000)
define(IDLE,60*10*1000)

def accept_connections(S) do
  {ok, ClientS} = gen_tcp:accept(S)
  spawn(fun() -> accept_connections(S) end)
  receive {tcp,_,Bin} ->
    gen_tcp:send(ClientS, websockets:handshake(Bin))
      step2(ClientS)
  after ?TIMEOUT ->
      websockets:die(ClientS,"Timeout on Handshake")
  end
end

def step2(ClientS) do
  receive {tcp,_,Bin1} ->
    ["register",User,Sprite,X,Y] = string:tokens(binary_to_list(binary:part(Bin1,1,byte_size(Bin1)-2)),"||")
    if (length(User)>25) -> websockets:die("Name too long"); true -> void end
    Test = lists:any(fun(E) when E===$ ;E===$<;E===$> -> true;(_)->false end,User)
    case Test do
      true -> websockets:die("Bad characters in username.'"); false -> void end
    {ok,{IP,_}} = inet:peername(ClientS)
    State = user{user=User,sprite=Sprite,sock=ClientS,x=X,y=Y,ip=IP,pid=self()}
    case es_websock:checkUser(State) do
      fail -> websockets:die(ClientS,"Already Connected");
      ID -> client(simple{id=ID,sock=ClientS})
    end
  after ?TIMEOUT ->
    websockets:die(ClientS,"Timeout on Handshake")
  end
end
   
def client(State) do
  receive
    {tcp,_,Bin} -> 
      Bin1 = binary_to_list(binary:part(Bin,1,byte_size(Bin)-2))
      actions(State,string:tokens(Bin1,"||"))
      client(State)
    {tcp_closed,_} ->
      logoutAndDie(State,"Disconnected")
    {die,Reason} ->
      logoutAndDie(State,Reason)
    What ->
      u:trace(What)
      logoutAndDie(State,"Crash")
    after ?IDLE ->
      logoutAndDie(State,"Idle")
  end
end

def logoutAndDie(State,MSG) do
    es_websock:logout(State),
    websockets:die(State simple.sock,MSG)
end
    
def actions(State,Data) do
    case Data do
        ["move",X,Y]  -> es_websock:move(State,X,Y);
        ["say",Message] -> es_websock:say(State,Message);
        ["nick",Name] -> es_websock:nick(State,Name);
        ["sprite",Sprite] when Sprite==="0";Sprite==="1" -> es_websock:sprite(State,Sprite);
        ["challenge",User]  -> es_websock:challenge(State,User);
        _ -> u:trace("Unidentified Message",Data)
    end
end



