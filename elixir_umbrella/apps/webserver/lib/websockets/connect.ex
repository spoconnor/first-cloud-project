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
      data = decodeString(bin1)
      recvMsg(clientS, data)
      step2(clientS)
    after timeoutTime ->
      Websocket.Websockets.die(clientS,"Timeout on Handshake")
  end
end

# RegisterClient = 1
def recvMsg(clientS, <<1, data>>) do
#def recvMsg(clientS, ["register",user,sprite,x,y]) do
  Lib.trace("Received: RegisterClient")
  msg = Messages.RegisterClientRequest.decode(data)
  Lib.trace("#{name}, #{ip}, #{pid}")
  reply = Messages.Status.new(status: :OK, message: "Registered")
  :gen_tcp.send(clientS, encodeString(Messages.Status.encode(reply)))
end

  if (length(user.name)>25) do
    Websocket.Websockets.die("Name too long")
  end
  
  #{:ok,{ip,_}} = :inet.peername(clientS)
  state = %Websocket.User{user: user.name, sock: clientS, x: 1,y: 1, ip: user.ip, pid: self()}

  case EsWebsock.checkUser(state) do
    fail -> Websocket.Websockets.die(clientS,"Already Connected");
    id -> client(%Websocket.Simple{id: id, sock: clientS})
  end
end

def decodeString(data) do
  decodeStream(:binary.bin_to_list(data))
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
  #masks = [:random.uniform(255), :random.uniform(255), 
  #         :random.uniform(255), :random.uniform(255)]
  #encoded = [129, Enum.count(msg) ||| 128] ++ masks
  encoded = [129, Enum.count(msg)] ++ msg
end
#def encodeBytes([],encoded) do
#  encoded
#end
#def encodeBytes(msg,encoded) do
#  [byte|msg2]=msg
#  encodeBytes(msg2, masks2++[mask], encoded ++ [byte ^^^ mask])
#end

def client(state) do
  receive do
    {tcp,_,bin} -> 
      data = decodeString(bin1)
      actions(state, data)
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
    
# Message = 3
def actions(state, <<3, data>>) do
  Lib.trace("Received: Message")
  msg = Messages.Message.decode(data)
  Lib.trace("#{msg.from}, #{msg.target}, #{msg.message}")
  EsWebsock.say(state, msg)
end

# Movement = 4
def actions(state, <<4, data>>) do
  Lib.trace("Received: Movement")
  msg = Messages.Movement.decode(data)
  Lib.trace("#{object}, #{from.x},#{from.y} #{to.x},#{to.y} #{speed}")
  EsWebsock.move(state, msg.to.x, msg.to.y)
end

# Action = 5
def actions(state, <<5, data>>) do
  Lib.trace("Recevied: Action")
  msg = Messages.Action.decode(data)
  Lib.trace("#{from}, #{target}, #{what}, #{with}")
end

# Object = 6
def actions(state, <<6, data>>) do
  Lib.trace("Recevied: Object")
  msg = Messages.Object.decode(data)
  Lib.trace("#{location.x},#{location.y}, #{type}, #{action}, #{destination.x},#{destination.y}, #{speed}")
end

def actions(state, <<data>>) do
  Lib.trace("Received: Unknown")
end

end
