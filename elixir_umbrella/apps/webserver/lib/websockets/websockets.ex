defmodule Websocket.Websockets do

# TODO
def allowedOrigin do
  [ "rp.eliteskills.com",
    "jimmyr.com",
    "localhost",
    "76.74.253.61.844"
  ]
end

# You give it a websockets handshake and it returns a proper response. Accepts a Fun as callback
# in order to parse things like cookies or protocol.
 
def handshake(bin) do
  handshake(bin,:false)
end
def handshake(bin,callback) do
    case :binary.split(bin,<<0x0d0a0d0a::32>>) do
        [httpRequest|[data]] -> :nil
        [httpRequest] -> data = :nil
    end
#TODO - fix
    #fields = :binary.split(httpRequest,<<0x0d0a::16>>,[global])
    fields = "wibble"
    %Websocket.Websock{
               key1: key1,
               key2: key2,
               origin: origin,
               request: request,
               host: host,
               port: port
            } = parseKeys(fields,%Websocket.Websock{allowed: allowedOrigin, callback: callback})

     case (key1===:undefined or key2===:undefined) do
         :false -> newWay=:true
         :true -> newWay=:false
     end
    ["HTTP/1.1 101 ",
     case newWay do 
       :true -> "WebSocket" 
       :false -> "Web Socket" 
     end,
     " Protocol Handshake\r\n",
     "Upgrade: WebSocket\r\n",
     "Connection: Upgrade\r\n",
     case newWay do
         :true ->
             ["Sec-WebSocket-Origin: ",origin,"\r\n",
              "Sec-WebSocket-Location: ws://",host,":",:erlang.integer_to_list(port),request,"\r\n",
              "Sec-WebSocket-Protocol: sample\r\n\r\n",
# TODO
#              :erlang.md5(<<key1::32, key2::32,data/binary>>)
             ]
         :false ->
             ["WebSocket-Origin: ",Origin,"\r\n",
              "WebSocket-Location: ws://",host,":",:erlang.integer_to_list(port),request,"\r\n\r\n"
             ]
     end
    ]
end

def alert(clientS,msg) do
  msg(clientS,"alert",msg)
end
def msg(clientS,msg) do
  :gen_tcp.send(clientS,[0,msg,255])
end
def msg(clientS,type,msg) do
  :gen_tcp.send(clientS,[0,type,<<" @@@ ">>,msg,255])
end

def die(clientS,msg) do
  alert(clientS,msg)
  :gen_tcp.send(clientS,[255,0])
  :gen_tcp.send(clientS,[0,0,0,0,0,0,0,0,0])
  :gen_tcp.close(clientS)
  Lib.trace(MSG)
end

#TODO
#def parseKeys([<<"Sec-WebSocket-key1: ",key/binary>>|t],websock) do
#  parseKeys(t,websock{key1=genKey(key,[],0)})
#end
#def parseKeys([<<"Sec-WebSocket-Key2: ",key/binary>>|t],websock) do
#  parseKeys(t,websock{key2=genKey(key,[],0)})
#end
#def parseKeys([<<"Origin: ",origin/binary>>|t],websock) do
#  parseKeys(t,websock{origin=origin})
#end
#def parseKeys([<<"Host: ",host/binary>>|t],websock) do
#  [host1,port] = :binary.split(host,<<?:>>)
#  parseKeys(t,websock{host=host1,port=list_to_integer(:binary.bin_to_list(port))})
#end
# TODO
#def parseKeys([<<"GET ",request/binary>>|t],websock) do
#  size = byte_size(request)-9
#  <<request1::size/binary,_/binary>> = request
#  parseKeys(t,websock{request = request1})
#end

def parseKeys([],w) 
#when 
# TODO - error
#  w.origin!==:undefined and w.host!==:undefined
   do

  case  w.allowed do
    any ->
      test=:true
    allowed ->
      [_|Origin] = :re.replace(w.origin,"http://(www\.)?","",[:caseless])
      test = :lists.any(fn(Host) when 
        Host===Origin -> :true 
        (_) -> :false 
      end, allowed)
  end

  case test do
    :true -> W
    :false ->
      Lib.trace(W)
      throw("No matching allowed hosts")
  end
end

def parseKeys([],w) do
  Lib.trace(w)
  throw("Missing Information")
end
def parseKeys([_|t],w) 
# TODO - error
#when w.callback!==:false 
do
  f=w.callback
#TODO  
#parseKeys(t,w{callbackData=f})
end
def parseKeys([_|t],websock) do
  parseKeys(t,websock)
end

#def genKey(<<x::8,rest/binary>>,numbers,spaces) when x>47 and x<58 do
#  genKey(rest,[x|numbers],spaces)
#end
def genKey(<<>>,numbers,spaces) do
#    Lib.trace("Key: ",numbers)
  :erlang.list_to_integer(:lists.reverse(numbers)) / spaces
end
#def genKey(<<?\s::8,rest/binary>>,numbers,spaces) do
#  genKey(rest,numbers,spaces+1)
#end
#def genKey(<<_::8,bin/binary>>,numbers,spaces) do
#  genKey(bin,numbers,spaces)
#end

end
