defmodule Websocket.EsWebsock do
use GenServer

# This code uses erlang OTP's gen_server to handle incoming data like movements, messages and registration
# All data is centralized in the server state and is lost on exit. At this time there is no centralized database
# though I don't plan on adding features that require one, like a player inventory or permanent statistics.
#
# There is a performance penalty from using modules, but putting all the code here gets ridiculous pretty fast
# It's hard to debug and read things when everything is nested, stacking functions on one page isn't any prettier


def start_link(opts \\ []) do
  GenServer.start_link(__MODULE__, :ok, opts)
end

def init([]) do
  process_flag(:trap_exit, :true)
  #443
  case :gen_tcp.listen(844, [:binary, {:packet, 0}, {:active, :true}, {:reuseaddr, :true}, {:packet_size,1024*2},{:keepalive,:true}]) do
    {:ok, s} -> 
      spawn(fn() -> :connect.accept_connections(s) end)
      {:ok,state{sock=s}}
    Err -> 
      Lib.trace("Accept connections failed")
      throw(Err)
  end
end

def debug() do
  GenServer.call(:debug)
end

def stop() do
  GenServer.call(:die)
end

def gs() do
  GenServer.call(:getState)
end

def rs() do
  GenServer.call(:resetState)
end


def sendToAll(dict,you,message) do
  :dict.map(fn(id,_) when id===you -> void
    (_,Record) -> :gen_tcp.send(user.sock,[0,message,255])
    end,Dict)
end

def say(simple,message) do
  GenServer.cast({say,simple,message})
end

def move(simple,x,y) do
  GenServer.cast({move,simple,x,y})
end

def logout(simple) do
  GenServer.cast({logout,simple})
end

def checkUser(state) do
  GenServer.call({checkUser,state})
end

def handle_call({checkUser,userState}, _, state) do
  CheckUser.checkUser(userState,state)
end
def handle_call(:getState, _from, state) do
  {reply,state,state}
end
def handle_call(:debug, _from, state) do
  %State{lookupByID: lbid,lookupByName: lbName,lookupByIP: lbip,maps: maps} = state
  Lib.trace(:dict.to_list(:array.get(0,maps)))
  Lib.trace(:gb_trees.to_list(lbName))
  Lib.trace(:gb_trees.to_list(lbip))
  Lib.trace(:dict.to_list(lbid))
  {reply,:ok,state}
end
def handle_call(:resetState, _from, _state) do
  {reply,:ok,state{}}
end
def handle_call(:die, _from, state) do
  {stop, normal, state}
end
def handle_call(_request, _from, state) do
  Lib.trace("unknown gen_server:handle_call()",_request)
  {reply, :ok, state}
end

def handle_cast({say,simple,message}, state) when message!==""  do
  Say:say(simple,message,state)
end

handle_cast({move,simple,x,y}, state) do
  Move:move(simple,x,y,state)
end

def handle_cast({logout,simple}, state) do
  Logout:logout(simple,state)
end

def handle_cast(_msg, state) do
  Lib.trace("gen_server:cast()",_msg)
  {noreply, state}
end

def handle_info(_info, state) do
  Lib.trace("gen_server:handle_info()",_info)
  {noreply, state}
end

def terminate(_reason, %State{sock: sock} = state) do
  :gen_tcp.close(sock)
  Lib.trace("gen_server:terminate()",{_reason,state})
  :ok
end

def code_change(_oldVsn, state, _extra) do
  {:ok, state}
end

end
