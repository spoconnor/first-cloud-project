defmodule Websocket.Connect do

defmacro timeoutTime do
  30*1000
end
defmacro idleTime do
 60*10*1000
end

def accept_connections(s) do
  Lib.trace("Accept_Connections")
  {:ok, clientS} = :gen_tcp.accept(s)
  spawn(fn() -> accept_connections(s) end)
  receive do
    {tcp,_,bin} ->
      reply =  Websocket.Websockets.handshake(bin)
      Lib.trace("Reply: #{reply}")
      :gen_tcp.send(clientS, reply)
      step2(clientS)
    after timeoutTime ->
      Websocket.Websockets.die(clientS, "Timeout on Handshake")
  end
end

def step2(clientS) do
  Lib.trace("Connection Step2")
  receive do
    {tcp, _, bin1} ->
      Lib.trace("Received connection")
      msg = :string.tokens(:binary.bin_to_list(:binary.part(bin1,1,byte_size(bin1)-2)),"||") 
      recvMsg(clientS, msg)
      
    after timeoutTime ->
      Websocket.Websockets.die(clientS,"Timeout on Handshake")
  end
end
   
def recvMsg(clientS, ["register",user,sprite,x,y]) do
  Lib.trace("Registering user")
  if (length(user)>25) do
    Websocket.Websockets.die("Name too long")
  end
  #test = :lists.any(fn(e) -> e===$ | e===$< | e===$> end, user)
  #case test do
  #  true -> Websocket.Websockets.die("Bad characters in username.'")
  #  false -> void 
  #end
  {:ok,{ip,_}} = :inet.peername(clientS)
  state = %Websocket.User{user: user,sprite: sprite,sock: clientS,x: x,y: y,ip: ip,pid: self()}
  case EsWebsock.checkUser(state) do
    fail -> Websocket.Websockets.die(clientS,"Already Connected");
    id -> client(%Websocket.Simple{id: id, sock: clientS})
  end
end

def recvMsg(["ping"]) do
  Lib.trace("Ping")
  # TODO pong
end


def client(state) do
  receive do
    {tcp,_,bin} -> 
      bin1 = :binary.bin_to_list(:binary.part(bin,1,byte_size(bin)-2))
      actions(state, :string.tokens(bin1,"||"))
      client(state)
    {:tcp_closed,_} ->
      logoutAndDie(state,"Disconnected")
    {:die,reason} ->
      logoutAndDie(state,reason)
    what ->
      Lib.trace(what)
      logoutAndDie(state,"Crash")
    after idleTime ->
      logoutAndDie(state,"Idle")
  end
end

def logoutAndDie(state,msg) do
    EsWebsock.logout(state)
    Websocket.Websockets.die(state.sock,msg)
end
    
def actions(state,data) do
    case data do
        ["move",x,y]  -> EsWebsock.move(state,x,y)
        ["say",message] -> EsWebsock.say(state,message)
        ["nick",name] -> EsWebsock.nick(state,name)
        ["sprite",sprite] when sprite==="0" or sprite==="1" -> EsWebsock.sprite(state,sprite)
        ["challenge",user]  -> EsWebsock.challenge(state,user)
        _ -> Lib.trace("Unidentified Message",data)
    end
end

end
