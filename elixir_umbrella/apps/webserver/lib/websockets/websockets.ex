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
    Lib.trace("Handshaking...")
    Lib.trace(bin)
    #split = :binary.split(bin,<<0x0d0a0d0a::32>>) 
    #case split do
    #    [httpRequest|[data]] -> :nil
    #    [httpRequest|data] -> :nil
    #    [httpRequest] -> data = :nil
    #    httpRequest -> httpRequest = :nil
    #             data = :nil
    #end
    httpRequest = bin
    #data = :nil
    #Lib.trace("HttpRequest: #{httpRequest}")
    #Lib.trace("HttpData: #{data}")
    #fields = :binary.split(httpRequest,<<0x0d0a20::16>>,[:global])
    fields = String.split(httpRequest, [" ", "\r\n"]) #<<0x0d0a::16>>])
    #:lists.foreach(fn(a) -> Lib.trace("Field: #{a}") end, fields)
    Lib.trace("Fields: #{fields}")
    %Websocket.Websock{
               key: key,
               key1: _key1,
               key2: _key2,
               version: _version,
               protocol: _protocol,
               origin: origin,
               request: _request,
               host: _host,
               port: _port
            } = parseKeys(fields,%Websocket.Websock{allowed: allowedOrigin, callback: callback})

     #case (key1===:nil or key2===:nil) do
     #    :false -> newWay=:true
     #    :true -> newWay=:false
     #end

    # TODO - filter unsupported protocols

    acceptKey = :base64.encode(:crypto.hash(:sha, <<"#{key}258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>))

    ["HTTP/1.1 101 ", 
     #"WebSocket",
      "Switching Protocols\r\n",
     #case newWay do 
     #  :true -> "WebSocket" 
     #  :false -> "Web Socket" 
     #end,
     #"Protocol Handshake\r\n",
     "Upgrade: websocket\r\n",
     "Connection: Upgrade\r\n",
     #case newWay do
     #    :true ->
     #       [
             "Sec-WebSocket-Origin: #{origin}\r\n",
     #         "Sec-WebSocket-Location: ws://#{host}:#{port}\r\n",
              "Sec-WebSocket-Accept: #{acceptKey}\r\n",
              #"Sec-WebSocket-Protocol: sample\r\n",
              #"#{request}"
     #         :erlang.md5(<<key1::32, key2::32,data>>)
     #        ]
     #    :false ->
     #        ["WebSocket-Origin: #{Origin}\r\n",
     #         "WebSocket-Location: ws://#{host}:#{port}#{request}\r\n\r\n"
     #        ]
              "\r\n"
     #end
    ]
end

def sendTcpMsg(clientS, msg) do
  :gen_tcp.send(clientS, encodeString(msg))
end

def encodeString(msg) do
encodeStream(:binary.bin_to_list(msg))
end
def encodeStream(msg) do
  #masks = [:random.uniform(255), :random.uniform(255),
  #         :random.uniform(255), :random.uniform(255)]
  #[129, Enum.count(msg) ||| 128] ++ masks
  [129, Enum.count(msg)] ++ msg
end
#def encodeBytes([],encoded) do
#  encoded
#end
#def encodeBytes(msg,encoded) do
#  [byte|msg2]=msg
#  encodeBytes(msg2, masks2++[mask], encoded ++ [byte ^^^ mask])
#end


def alert(clientS,msg) do
  msg(clientS,"alert",msg)
end
def msg(clientS,msg) do
  IO.puts("Sending msg '#{msg}'")
  #:gen_tcp.send(clientS,[0,msg,255])
  sendTcpMsg(clientS, msg)
end
def msg(clientS,type,msg) do
  IO.puts("Sending msg '#{msg}' type '#{type}' to '#{:erlang.port_info(clientS)[:id]}'")
  #:gen_tcp.send(clientS,[0,type,<<" @@@ ">>,msg,255])
  sendTcpMsg(clientS, "Testing...")
  sendTcpMsg(clientS, msg)
end

def die(clientS,msg) do
  IO.puts("Websockets die '#{:erlang.port_info(clientS)[:id]}'")
  alert(clientS,msg)
  #:gen_tcp.send(clientS,[255,0])
  #:gen_tcp.send(clientS,[0,0,0,0,0,0,0,0,0])
  :gen_tcp.close(clientS)
  Lib.trace(MSG)
end

def parseKeys(["GET","/",request|t],websock) do
  Lib.trace("ParseKeys Get: #{request}")
  #size = :binary.byte_size(request)-9
  #<<request1::size,_>> = request
  parseKeys(t,%{websock | request: request})
end

def parseKeys(["Host:",host|t],websock) do
  Lib.trace("ParseKeys Host: #{host}")
  uri=URI.parse("ws://{host}")
  parseKeys(t,%{websock | host: uri.host, port: uri.port})
end

def parseKeys(["Upgrade:","websocket"|t],websock) do
  Lib.trace("ParseKeys Upgrade: websocket")
  parseKeys(t,websock)
end

def parseKeys(["Sec-WebSocket-Protocol:",protocol|t],websock) do
  Lib.trace("ParseKeys Sec-WebSocket-Protocol: #{protocol}")
  parseKeys(t,%{websock | protocol: protocol})
end

def parseKeys(["Sec-WebSocket-Key:",key|t],websock) do
  Lib.trace("ParseKeys Sec-WebSocket-Key: #{key}")
  #parseKeys(t,%{websock | key: genKey(key,[],0)})
  parseKeys(t,%{websock | key: key})
end
#def parseKeys(["Sec-WebSocket-key1:",key|t],websock) do
#  Lib.trace("ParseKeys Sec-WebSocket-Key1: #{key}")
#  parseKeys(t,%{websock | key1: genKey(key,[],0)})
#end
#def parseKeys(["Sec-WebSocket-Key2:",key|t],websock) do
#  Lib.trace("ParseKeys Sec-WebSocket-Key2: #{key}")
#  parseKeys(t,%{websock | key2: genKey(key,[],0)})
#end

def parseKeys(["Sec-WebSocket-Version:",version|t],websock) do
  Lib.trace("ParseKeys Sec-WebSocket-Version: #{version}")
  parseKeys(t,%{websock | version: version})
end

def parseKeys(["Origin:",origin|t],websock) do
  Lib.trace("ParseKeys Origin: #{origin}")
  parseKeys(t,%{websock | origin: origin})
end
def parseKeys([], %Websocket.Websock{origin: :undefined, host: :undefined} = _w) do
  Lib.trace("ParseKeys Undefined")
  :nil
end
def parseKeys([], %Websocket.Websock{} = w)
  do
  Lib.trace("ParseKeys end")

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
    :true -> w
    :false ->
      Lib.trace(w)
      throw("No matching allowed hosts")
  end
end

def parseKeys([],w) do
  Lib.trace("ParseKeys [] #{w}")
  throw("Missing Information")
end

def parseKeys([ignore|t], %Websocket.Websock{callback: :false} = w) do
  Lib.trace("ParseKeys Ignoring [#{ignore}|t] callback=false")
  parseKeys(t,w)
end
def parseKeys([ignore|t], %Websocket.Websock{} = w) do
  Lib.trace("ParseKeys Ignoring [#{ignore}|t]")
  f=w.callback
  parseKeys(t, %{w | callbackData: f})
end

#def genKey(<<x::8,rest>>,numbers,spaces) when x>47 and x<58 do
#  genKey(rest,[x|numbers],spaces)
#end
#def genKey(<<>>,numbers,spaces) do
#  Lib.trace("Key: ",numbers)
#  :erlang.list_to_integer(:lists.reverse(numbers)) / spaces
#end
#def genKey(<<?\s::8,rest>>,numbers,spaces) do
#  genKey(rest,numbers,spaces+1)
#end
#def genKey(<<_::8,bin>>,numbers,spaces) do
#  genKey(bin,numbers,spaces)
#end

end
