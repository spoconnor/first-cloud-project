defmodule Websockets.Websockets do

defrecord(websock,{key1,key2,allowed,origin,host,request,port,callback,callbackData=[]})
end

define(AllowedOrigin,
        [ <<"rp.eliteskills.com">>
              , <<"jimmyr.com">>
              , <<"localhost">>
              , <<"76.74.253.61.844">>
        ])
end

%You give it a websockets handshake and it returns a proper response. Accepts a Fun as callback
%in order to parse things like cookies or protocol.
 
def handshake(Bin) do
  handshake(Bin,false)
end
def handshake(Bin,Callback) do
    case binary:split(Bin,<<16#0d0a0d0a:32>>) do
        [HttpRequest|[Data]] -> void;
        [HttpRequest] -> Data = void
    end
    Fields = binary:split(HttpRequest,<<16#0d0a:16>>,[global]),
    websock{
                key1=Key1
              , key2=Key2
              , origin=Origin
              , request=Request
              , host=Host
              , port=Port
            } = parseKeys(Fields,websock{allowed=?AllowedOrigin,callback=Callback})

     case (Key1===undefined orelse Key2===undefined) do
         false -> NewWay=true;
         true -> NewWay=false
     end
    ["HTTP/1.1 101 ",case NewWay do true-> "WebSocket"; false-> "Web Socket" end," Protocol Handshake\r\n",
     "Upgrade: WebSocket\r\n",
     "Connection: Upgrade\r\n",
     case NewWay do
         true ->
             ["Sec-WebSocket-Origin: ",Origin,"\r\n",
              "Sec-WebSocket-Location: ws://",Host,":",integer_to_list(Port),Request,"\r\n",
              "Sec-WebSocket-Protocol: sample\r\n\r\n",
              erlang:md5(<<Key1:32, Key2:32,Data/binary>>)
             ];
         false ->
             ["WebSocket-Origin: ",Origin,"\r\n",
              "WebSocket-Location: ws://",Host,":",integer_to_list(Port),Request,"\r\n\r\n"
             ]
     end
    ]
end

def alert(ClientS,MSG) do
  msg(ClientS,"alert",MSG)
end
def msg(ClientS,MSG) do
  gen_tcp:send(ClientS,[0,MSG,255])
end
def msg(ClientS,Type,MSG) do
  gen_tcp:send(ClientS,[0,Type,<<" @@@ ">>,MSG,255])
end

def die(ClientS,MSG) do
  alert(ClientS,MSG)
  gen_tcp:send(ClientS,[255,0])
  gen_tcp:send(ClientS,[0,0,0,0,0,0,0,0,0])
  gen_tcp:close(ClientS)
  u:trace(MSG)
end

def parseKeys([<<"Sec-WebSocket-Key1: ",Key/binary>>|T],Websock) 
  parseKeys(T,Websock websock{key1=genKey(Key,[],0)})
end
def parseKeys([<<"Sec-WebSocket-Key2: ",Key/binary>>|T],Websock) ->
  parseKeys(T,Websock websock{key2=genKey(Key,[],0)})
end
def parseKeys([<<"Origin: ",Origin/binary>>|T],Websock) do
  parseKeys(T,Websock websock{origin=Origin})
end
def parseKeys([<<"Host: ",Host/binary>>|T],Websock) ->
  [Host1,Port] = binary:split(Host,<<$:>>),
  parseKeys(T,Websock websock{host=Host1,port=list_to_integer(binary_to_list(Port))})
end
def parseKeys([<<"GET ",Request/binary>>|T],Websock) do
  Size = byte_size(Request)-9,
  <<Request1:Size/binary,_/binary>> = Request,
  parseKeys(T,Websock websock{request = Request1})
end
def parseKeys([],W) when 
  W websock.origin!==undefined and W websock.host!==undefined
    ->

  case  W websock.allowed do
    any ->
      Test=true;
    Allowed ->
      [_|Origin] = re:replace(W websock.origin,"http://(www\.)?","",[caseless]),
      Test = lists:any(fun(Host) when Host===Origin -> true; (_) -> false end,Allowed)
  end

  case Test do
    true -> W;
    false ->
      u:trace(W),
      throw("No matching allowed hosts")
  end
end

def parseKeys([],W) do
  u:trace(W)
  throw("Missing Information")
end
def parseKeys([_|T],W) when W websock.callback!==false do
  F=W websock.callback
  parseKeys(T,W websock{callbackData=F()})
end
def parseKeys([_|T],Websock) do
  parseKeys(T,Websock)
end

def genKey(<<X:8,Rest/binary>>,Numbers,Spaces) when X>47 and X<58 do
  genKey(Rest,[X|Numbers],Spaces)
end
def genKey(<<>>,Numbers,Spaces) do
%    u:trace("Key: ",Numbers)
  list_to_integer(lists:reverse(Numbers)) div Spaces
end
def genKey(<<$ :8,Rest/binary>>,Numbers,Spaces) do
  genKey(Rest,Numbers,Spaces+1)
end
def genKey(<<_:8,Bin/binary>>,Numbers,Spaces) do
  genKey(Bin,Numbers,Spaces)
end


