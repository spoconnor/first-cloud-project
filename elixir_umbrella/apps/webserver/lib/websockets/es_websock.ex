defmodule es_websock do
use GenServer

%This code uses erlang OTP's gen_server to handle incoming data like movements, messages and registration
%All data is centralized in the server state and is lost on exit. At this time there is no centralized database
%though I don't plan on adding features that require one, like a player inventory or permanent statistics.

%There is a performance penalty from using modules, but putting all the code here gets ridiculous pretty fast
%It's hard to debug and read things when everything is nested, stacking functions on one page isn't any prettier


def start_link() do
  gen_server:start_link({:local, ?SERVER}, ?MODULE, [], [])
end

def init([]) do
  process_flag(:trap_exit, :true)
  %443
  case gen_tcp:listen(844, [:binary, {:packet, 0}, {:active, :true}, {:reuseaddr, :true}, {:packet_size,1024*2},{:keepalive,:true}]) of
    {:ok, S} -> 
      spawn(fun() -> connect:accept_connections(S) :end)
      {:ok,#state{sock=S}}
    Err -> 
      u:trace("Accept connections failed")
      throw(Err)
  end
end

def debug() do
  gen_server:call(?MODULE,debug)
end

def stop() do
  gen_server:call(?MODULE,die)
end

def gs() do
  gen_server:call(?MODULE,getState)
end

def rs() do
  gen_server:call(?MODULE,resetState)
end


def sendToAll(Dict,You,Message) do
  dict:map(fun(ID,_) when ID=:=You -> void
    (_,Record) -> gen_tcp:send(Record#user.sock,[0,Message,255])
    end,Dict)
end

def say(Simple,Message) do
  gen_server:cast(?MODULE,{say,Simple,Message})
end

def move(Simple,X,Y) do
  gen_server:cast(?MODULE,{move,Simple,X,Y})
end

def logout(Simple) do
  gen_server:cast(?MODULE,{logout,Simple})
end

def checkUser(State) do
  gen_server:call(?MODULE,{checkUser,State})
end

def handle_call({checkUser,UserState}, _, State) do
  checkUser:checkUser(UserState,State)
end
def handle_call(getState, _From, State) do
  {reply,State,State}
end
def handle_call(debug, _From, State) do
  #state{lookupByID=LBID,lookupByName=LBName,lookupByIP=LBIP,maps=Maps} = State
  u:trace(dict:to_list(array:get(0,Maps)))
  u:trace(gb_trees:to_list(LBName))
  u:trace(gb_trees:to_list(LBIP))
  u:trace(dict:to_list(LBID))
  {reply,:ok,State}
end
def handle_call(resetState, _From, _State) do
  {reply,:ok,#state{}}
end
def handle_call(die, _From, State) do
  {stop, normal, State}
end
def handle_call(_Request, _From, State) do
  u:trace("unknown gen_server:handle_call()",_Request)
  {reply, :ok, State}
end

def handle_cast({say,Simple,Message}, State) when Message=/=""  do
  say:say(Simple,Message,State)
end

handle_cast({move,Simple,X,Y}, State) do
  move:move(Simple,X,Y,State)
end

def handle_cast({logout,Simple}, State) do
  logout:logout(Simple,State)
end

def handle_cast(_Msg, State) do
  u:trace("gen_server:cast()",_Msg)
  {noreply, State}
end

def handle_info(_Info, State) do
  u:trace("gen_server:handle_info()",_Info)
  {noreply, State}
end

def terminate(_Reason, #state{sock=Sock} = State) do
  gen_tcp:close(Sock)
  u:trace("gen_server:terminate()",{_Reason,State})
  :ok
end

def code_change(_OldVsn, State, _Extra) do
  {:ok, State}
end
