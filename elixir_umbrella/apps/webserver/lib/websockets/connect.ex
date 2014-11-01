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
def registerMsg(clientS, ["register",msg]) do
  Lib.trace("Received: RegisterClient #{msg}")
  user = CommsMessages.RegisterClientRequest.decode(msg)
  Lib.trace("#{user.name}")
  #Lib.trace("#{data}")
  #fields=String.split(data, [" ", "\r\n"])
  #Lib.trace("#{fields}")

  #if (length(user.name)>25) do
  #  Websocket.Websockets.die("Name too long")
  #end
  
  {:ok,{ip,_}} = :inet.peername(clientS)
  state = %Websocket.User{user: user.name, sock: clientS, x: 1,y: 1, ip: ip, pid: self()}

  #reply = Messages.Status.new(status: :OK, message: "Registered")
  reply = "Registered"
  Websocket.Websockets.sendTcpMsg(clientS, reply)
  notify_pid = spawn(fn() -> notify_thread(clientS) end)
  Websocket.Users.add_user(Websocket.Users, user.name, notify_pid)

  case Websocket.EsWebsock.checkUser(Websocket.Worker, state) do
    {:fail, _} -> Websocket.Websockets.die(clientS,"Already Connected");
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

def client(state) do
  IO.puts("Client #{state.id} receive loop")
  receive do
    {tcp,_,bin} -> 
      str = to_string(decodeString(bin))
      IO.puts "'#{str}'"
      data = String.split(str, "|")

      #actions(state, data)
      # Temp code to send message thru rabbit queue
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
    {data} ->
      IO.puts("Client notify thread recd #{data}")
      notify_thread(clientS)
  end
end

def logoutAndDie(state,msg) do
    Websocket.EsWebsock.logout(Websocket.Worker, state)
    Websocket.Websockets.die(state.sock,msg)
end
    
def actions(state, ["say",data]) do
  Lib.trace("Received: Message")
  msg = CommsMessages.Message.decode(data)
  Lib.trace("#{msg.from}, #{msg.target}, #{msg.message}")
  Websocket.EsWebsock.say(Websocket.Worker, state, msg.message)
end

def actions(state, ["move",data]) do
  Lib.trace("Received: Movement")
  msg = CommsMessages.Movement.decode(data)
  Lib.trace("#{msg.object}, #{msg.from.x},#{msg.from.y} #{msg.to.x},#{msg.to.y} #{msg.speed}")
  Websocket.EsWebsock.move(Websocket.Worker, state, msg.to.x, msg.to.y)
end

def actions(state, ["action"|data]) do
  Lib.trace("Recevied: Action")
  #msg = Messages.Action.decode(data)
  #Lib.trace("#{msg.from}, #{msg.target}, #{msg.what}, #{msg.with}")
end

def actions(state, ["object"|data]) do
  Lib.trace("Recevied: Object")
  #msg = Messages.Object.decode(data)
  #Lib.trace("#{msg.location.x},#{msg.location.y}, #{msg.type}, #{msg.action}, #{msg.destination.x},#{msg.destination.y}, #{msg.speed}")
end

def actions(state, _unknown) do
  Lib.trace("Received: Unknown")
end

end
