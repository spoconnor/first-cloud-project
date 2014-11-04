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
    {_tcp,_,bin} ->
      reply =  Websocket.Websockets.handshake(bin)
      Lib.trace("Reply: #{reply} to '#{:erlang.port_info(clientS)[:id]}'")
      Lib.trace(:erlang.port_info(clientS))
      :gen_tcp.send(clientS, reply)
      #Websocket.Websockets.sendTcpMsg(clientS, reply)
      step2(clientS)
    after timeoutTime ->
      Websocket.Websockets.die(clientS, "Timeout on Handshake")
  end
end

def step2(clientS) do
  Lib.trace("Connection Step2")
  receive do
    {_tcp, _, bin1} ->
      str = to_string(decodeString(bin1))
      Lib.trace("Received:", str)
      msg = CommsMessages.Base.decode(str)
      registerMsg(clientS, msg.msgtype, msg)
    after timeoutTime ->
      Websocket.Websockets.die(clientS,"Timeout on Handshake")
  end
end

# RegisterClient
def registerMsg(clientS, msgtype, msg) do
  Lib.trace("Received:", msg.msgtype)
  register = msg.register
  Lib.trace("Registering #{register.name}")
  #if (length(user.name)>25) do
  #  Websocket.Websockets.die("Name too long")
  #end
  {:ok,{ip,_}} = :inet.peername(clientS)
  state = %Websocket.User{user: register.name, sock: clientS, x: 1,y: 1, ip: ip, pid: self()}

  notify_pid = spawn(fn() -> notify_thread(clientS) end)
  Websocket.Users.add_user(Websocket.Users, register.name, notify_pid)

  case Websocket.EsWebsock.checkUser(Websocket.Worker, state) do
    {:fail, _} -> Websocket.Websockets.die(clientS,"Already Connected");
    id ->
      Lib.trace("ObjectId: #{id}")
      reply = CommsMessages.Base.new(msgtype: :'CommsMessages.Base.MsgType.ERegistered', registered: CommsMessages.Base.Registered.new(objectid: id, motd: "Welcome!"))
      data = CommsMessages.Base.encode(reply)
      Websocket.Websockets.sendTcpMsg(clientS, data)
      client(%Websocket.Simple{id: id, sock: clientS})
  end
end

#def registerMsg(clientS, unexpected, _data) do
#  Lib.trace("Unexpected type received", unexpected)
#end

def decodeString(data) do
  decodeStream(:binary.bin_to_list(data))
end
# encrypted string marker
def decodeStream([129,b2|t]) do
  #length = b2 &&& 127  # bitwise AND, unless special case
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

def decodeBytes([],_masks,decoded) do
  decoded
end
def decodeBytes(data,masks,decoded) do
  [byte|data2]=data
  [mask|masks2]=masks
  decodeBytes(data2, masks2++[mask], decoded ++ [byte ^^^ mask])
end

def client(state) do
  IO.puts("Client #{state.id} receive loop")
  receive do
    {_tcp,_,bin} -> 
      str = to_string(decodeString(bin))
      Lib.trace("received:", str)
      data = String.split(str, "|")

      actions(state, data)
      # Send message thru rabbit queue
      {:ok, conn} = AMQP.Connection.open
      {:ok, chan} = AMQP.Channel.open(conn)
      AMQP.Basic.publish chan, "webserver_exchange", "", str

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

def notify_thread(clientS) do
  IO.puts("Client notify_thread")
  receive do
    data ->
      IO.puts("Client notify thread recd #{data}")
      Websocket.Websockets.sendTcpMsg(clientS, data)
      notify_thread(clientS)
  end
end

def logoutAndDie(state,msg) do
    Websocket.EsWebsock.logout(Websocket.Worker, state)
    Websocket.Websockets.die(state.sock,msg)
end
    
def actions(_state, ["say",data]) do
  Lib.trace("Received: Say")
  msg = CommsMessages.Base.Say.decode(data)
  Lib.trace("#{msg.from}, #{msg.target}, #{msg.text}")
  #Websocket.EsWebsock.say(Websocket.Worker, state, msg.message)
end

def actions(state, ["move",data]) do
  Lib.trace("Received: Movement")
  msg = CommsMessages.Base.Movement.decode(data)
  Lib.trace("#{msg.object}, #{msg.from.x},#{msg.from.y} #{msg.to.x},#{msg.to.y} #{msg.speed}")
  #Websocket.EsWebsock.move(Websocket.Worker, state, msg.to.x, msg.to.y)
end

def actions(_state, ["action"|_data]) do
  Lib.trace("Recevied: Action")
  #msg = Messages.Action.decode(data)
  #Lib.trace("#{msg.from}, #{msg.target}, #{msg.what}, #{msg.with}")
end

def actions(_state, ["object"|_data]) do
  Lib.trace("Recevied: Object")
  #msg = Messages.Object.decode(data)
  #Lib.trace("#{msg.location.x},#{msg.location.y}, #{msg.type}, #{msg.action}, #{msg.destination.x},#{msg.destination.y}, #{msg.speed}")
end

def actions(_state, _unknown) do
  Lib.trace("Received: Unknown")
end

end
