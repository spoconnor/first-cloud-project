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
      IO.puts "Received something..."
      str = to_string(decodeString(bin1))
      IO.puts "'#{str}'"
      data = String.split(str, "|")
      registerMsg(clientS, data)
    after timeoutTime ->
      Websocket.Websockets.die(clientS,"Timeout on Handshake")
  end
end

# RegisterClient = 1
def registerMsg(clientS, ["register",name]) do
  Lib.trace("Received: RegisterClient #{name}")
  #user = Messages.RegisterClientRequest.decode(data)
  #Lib.trace("#{user.name}, #{user.ip}, #{user.pid}")
  #Lib.trace("#{data}")
  #fields=String.split(data, [" ", "\r\n"])
  #Lib.trace("#{fields}")

  #if (length(user.name)>25) do
  #  Websocket.Websockets.die("Name too long")
  #end
  
  {:ok,{ip,_}} = :inet.peername(clientS)
  state = %Websocket.User{user: name, sock: clientS, x: 1,y: 1, ip: ip, pid: self()}

  #reply = Messages.Status.new(status: :OK, message: "Registered")
  #:gen_tcp.send(clientS, encodeString(Messages.Status.encode(reply)))
  reply = "Registered"
  :gen_tcp.send(clientS, encodeString(reply))

  case Websocket.EsWebsock.checkUser(Websocket.EsWebsock, state) do
    fail -> Websocket.Websockets.die(clientS,"Already Connected");
    id -> client(%Websocket.Simple{id: id, sock: clientS})
  end
end

def decodeString(data) do
  decodeStream(:binary.bin_to_list(data))
end
# encrypted string marker
def decodeStream([129,b2|t]) do
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
  IO.puts("Client receive loop")
  receive do
    {tcp,_,bin} -> 
      data = decodeString(bin)
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
  Lib.trace("#{msg.object}, #{msg.from.x},#{msg.from.y} #{msg.to.x},#{msg.to.y} #{msg.speed}")
  EsWebsock.move(state, msg.to.x, msg.to.y)
end

# Action = 5
def actions(state, <<5, data>>) do
  Lib.trace("Recevied: Action")
  msg = Messages.Action.decode(data)
  Lib.trace("#{msg.from}, #{msg.target}, #{msg.what}, #{msg.with}")
end

# Object = 6
def actions(state, <<6, data>>) do
  Lib.trace("Recevied: Object")
  msg = Messages.Object.decode(data)
  Lib.trace("#{msg.location.x},#{msg.location.y}, #{msg.type}, #{msg.action}, #{msg.destination.x},#{msg.destination.y}, #{msg.speed}")
end

def actions(state, <<data>>) do
  Lib.trace("Received: Unknown")
end

end
