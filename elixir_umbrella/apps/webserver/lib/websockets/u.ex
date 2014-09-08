defmodule(u) do

def say(X) do
  spawn(fun() -> :io.format("~s~n",[X]) end)
end
def trace(X) do
  spawn(fun() -> :io.format("~p~n",[X]) end)
end
def trace(X,Y) do
  spawn(fun() -> :io.format("~s: ~p~n",[X,Y]) end)
end
def traceBinary(X) do
  spawn(fun() -> :io.format("~p~n",[b2h(X)]) end)
end
def for(Max, Max, F) do
  [F(Max)]
end
def for(I, Max, F) do
  [F(I)|for(I+1, Max, F)]
end
def b2h(Bin) do
  lists:flatten([:io_lib.format("~2.16.0B", [X]) || X <- binary_to_list(Bin)])
end
def h2b(String) do
  << << (erlang:list_to_integer([Char], 16)):4/integer >> || Char <- String >>
end
def txt(Bin) do
  [X || <<X>> <= Bin,X > 32, X < 127, X !== 45]
end
def b2s(Bin) do
  b2s1(binary_to_list(Bin),[])
end
def b2s1([],Str) do
  lists:reverse(Str)
end
def b2s1([H|T],Str) do
  case H > 32 andalso H < 127 andalso H !== 45 do
  	true -> b2s1(T,[H,$.|Str]);
  	false -> b2s1(T,[46,46|Str])
  end
end

def pmap(F, L,Parent) do
  [receive {Pid, Res} -> Res end || Pid <- [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- L]]
end

def timer(Time,Fun) do
  spawn(fun() -> receive after Time -> ?MODULE:Fun() end end)
end

def signSubtract(A,B) do
  case A<0 do
    true -> (erlang:abs(A)-erlang:abs(B))*-1;
    false -> (erlang:abs(A)-erlang:abs(B))
  end
end

def signSubtract1(A,B) do
    case A<0 do
        true -> (erlang:abs(A)-B)*-1;
        _ -> (erlang:abs(A)-B)
    end
end

def floor(X) when X < 0 do
  T = trunc(X),
  case (X - T) === 0 do
    true -> T;
    false -> T - 1
  end
end
def floor(X) do
  trunc(X)
end

def addLen(Bin) do
  Len=erlang:size(Bin)+2,
  <<Len:16,Bin/binary>>
end

def datetime_to_unixtime({{_Year, _Month, _Day},{_Hour, _Min, _Sec}}=Datetime) do
  UnixZero = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
  Seconds = calendar:datetime_to_gregorian_seconds(Datetime),
  Seconds - UnixZero
end

def unixtime() do
  {MegaSecs, Secs, _MicroSecs} = erlang:now(),
  MegaSecs * 1000000 + Secs
end

def munixtime() do
  {MegaSecs, Secs, MicroSecs} = erlang:now(),
  MegaSecs * 1000000000 + Secs*1000 + (MicroSecs div 1000)
end

def unique(N) do
  unique(N,[])
end
def unique(0,L) do
  L
end
def unique(N,L) do
  Arr = [$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,$w,$x,$y,$z,$A,$B,$C,$D,$E,$F,$G,$H,$I,$J,$K,$L,$M,$N,$O,$P,$Q,$R,$S,$T,$U,$V,$W,$X,$Y,$Z,$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$-,$_],
  unique(N-1,[lists:nth(random:uniform(64),Arr)|L])
end
