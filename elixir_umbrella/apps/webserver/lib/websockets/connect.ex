defmodule Websocket.Connect do
use Bitwise

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
      #msg = :string.tokens(:binary.bin_to_list(:binary.part(bin1,1,byte_size(bin1)-2)),"||") 
      #<<msg::utf8>> = bin1
      msg = decodeString(bin1)
      recvMsg(clientS, msg)
      step2(clientS)
    after timeoutTime ->
      Websocket.Websockets.die(clientS,"Timeout on Handshake")
  end
end

def decodeString(data) do
  decodeStream(:binary.bin_to_list(data)
end
def decodeStream(bytes) do
  [129,b2|t] = bytes  # string marker
  length = b2 &&& 127  # bitwise AND, unless special case
  #indexFirstMask = 2   # if not a special case
  # TODO - For message length > 126 bytes
  #if (length == 126)   # if a special case, change indexFirstMask
  #  indexFirstMask = 4
  #else if length == 127  # special case 2
  #  indexFirstMask = 10
  #end
  [mask1,mask2,mask3,mask4|data] = t
  masks = [mask1,mask2,mask3,mask4]
  decodeBytes(data,masks,[])
end
def decodeBytes([],masks,decoded) do
  decoded
end
def decodeBytes(data,masks,decoded) do
  [byte|data2]=data
  [mask|masks2]=masks
  decodeBytes(data2, masks2++[mask], decoded ++ [byte ^^^ mask])
end

def encodeString(msg) do
  encodeStream(:binary.bin_to_list(msg))
end
def encodeStream(msg) do
  masks = [:random.uniform(255), :random.uniform(255), 
           :random.uniform(255), :random.uniform(255)]
  encoded = [129, Enum.count(msg) ||| 128] | masks
  encodeBytes(msg, masks, encoded)
end
def encodeBytes([],masks,encoded) do
  encoded
end
def encodeBytes(msg,masks,encoded) do
  [byte|msg2]=msg
  [mask|masks2]=masks
  encodeBytes(msg2, masks2++[mask], encoded ++ [byte ^^^ mask])
end

def recvMsg(clientS, msg) do
  Lib.trace("Received:")
  Lib.trace(msg)
  #msg2 = encodeStream(msg)
  :gen_tcp.send(clientS, msg)
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
  msg = encodeString("pong")
  :gen_tcp.send(clientS, msg)
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
