defmodule Websocket.Connect do

defmacro timeoutTime do
  30*1000
end
defmacro idleTime do
 60*10*1000
end

def accept_connections(s) do
  {:ok, clientS} = :gen_tcp.accept(s)
  spawn(fn() -> accept_connections(s) end)
  receive do
    {tcp,_,bin} ->
      :gen_tcp.send(clientS, :websockets.handshake(bin))
      step2(clientS)
    after timeoutTime ->
      :websockets.die(clientS, "Timeout on Handshake")
  end
end

def step2(clientS) do
  receive do
    {tcp, _, bin1} ->
      ["register",user,sprite,x,y] = :string.tokens(binary_to_list(:binary.part(bin1,1,byte_size(bin1)-2)),"||")
      if (length(user)>25) do
        :websockets.die("Name too long")
      end
      #test = :lists.any(fn(e) -> e===$ | e===$< | e===$> end, user)
      #case test do
      #  true -> :websockets.die("Bad characters in username.'")
      #  false -> void 
      #end
      {ok,{ip,_}} = :inet.peername(clientS)
      state = %User{user: user,sprite: sprite,sock: clientS,x: x,y: y,ip: ip,pid: self()}
      case EsWebsock.checkUser(state) do
        fail -> :websockets.die(clientS,"Already Connected");
        id -> client(simple{id=id,sock=clientS})
      end
    after timeoutTime ->
      :websockets.die(clientS,"Timeout on Handshake")
  end
end
   
def client(state) do
  receive do
    {tcp,_,bin} -> 
      bin1 = binary_to_list(:binary.part(bin,1,byte_size(bin)-2))
      actions(state, :string.tokens(bin1,"||"))
      client(state)
    {tcp_closed,_} ->
      logoutAndDie(state,"Disconnected")
    {die,reason} ->
      logoutAndDie(state,reason)
    What ->
      Lib.trace(what)
      logoutAndDie(state,"Crash")
    after idleTime ->
      logoutAndDie(state,"Idle")
  end
end

def logoutAndDie(state,msg) do
    EsWebsock.logout(state)
    :websockets.die(simple.sock,msg)
end
    
def actions(state,data) do
    case data do
        ["move",x,y]  -> :EsWebsock.move(state,x,y)
        ["say",message] -> :EsWebsock.say(state,message)
        ["nick",name] -> :EsWebsock.nick(state,name)
        ["sprite",sprite] when sprite==="0";sprite==="1" -> EsWebsock.sprite(state,sprite)
        ["challenge",user]  -> :EsWebsock.challenge(state,user)
        _ -> Lib.trace("Unidentified Message",data)
    end
end

end
