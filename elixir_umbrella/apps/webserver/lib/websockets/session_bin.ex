defmodule Websocket.SessionBin do

%This code can parse the PHP Session. For now, I'm just using it to see if the session exists
%in order to authenticate a particular name. Authenticated names will show up in a different
%color, while guests can still set nicknames.

%binary input
def session(Session1) do
  Session = re:replace(Session1,<<"[^a-z0-9]+">>,<<"">>,[global,{return,binary}]),

  case byte_size(Session) do
    26 -> void;
    _ -> throw("invalid session"),u:trace("Invalid Session")
  end,

  case file:read_file(["/var/lib/php5/sess_",binary_to_list(Session1)]) do
    {ok,Bin} -> parse(Bin);
    {error,_} -> throw("Could Not Load File")
  end
end

def parse(<<>>) do
 fail
end
def parse(S) do
  parseKey(S,<<>>,[])
end

def parseKey(<<>>,_,List) do
  lists:reverse(List)
end
def parseKey(<<$\|,S/binary>>,Key,List) do
  parseType(S,Key,List)
end
def parseKey(<<C,S/binary>>,Key,List) do
  parseKey(S,<<Key/binary,C>>,List)
end

def parseType(<<>>,_,_) do
  fail
end
def parseType(<<C,S/binary>>,Key,List) do
  case C do
    $i ->
      <<_,S1/binary>> = S,
      parseInt(S1,Key,<<>>,List);
    $s ->
      <<$:,S1/binary>> = S,
      parseStrLen(S1,<<>>,Key,List)
  end.

%def parseInt([],Key,Value,List) do
%  [{Key,list_to_integer(Value)}|List]
%end

def parseInt(<<$;,S/binary>>,Key,Value,List) do
  parseKey(S,<<>>,[{Key,binary_to_integer(Value,0)}|List])
end
def parseInt(<<C,S/binary>>,Key,Value,List) do
  parseInt(S,Key,<<Value/binary,C>>,List)
end

def parseStrLen(<<$:,$",T/binary>>,Len,Key,List) do
  parseString(binary_to_integer(Len,0),T,Key,<<>>,List)
end

def parseStrLen(<<C,T/binary>>,Len,Key,List) do
  parseStrLen(T,<<Len/binary,C>>,Key,List)
end

def parseString(0,<<$",$;,S/binary>>,Key,Value,List) do
  parseKey(S,<<>>,[{Key,Value}|List])
end

def parseString(Amount,<<C,S/binary>>,Key,Value,List) do
  parseString(Amount-1,S,Key,<<Value/binary,C>>,List)
end

def binary_to_integer(<<>>,Acc) do
  Acc
end
def binary_to_integer(<<Num:8,Rest/binary>>,Acc) when Num >= 48 and Num < 58 do
 binary_to_integer(Rest, Acc*10 + (Num-48))
end
def binary_to_integer(_,Acc) do
  exit({badarg,Acc})
end

end
