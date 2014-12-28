defmodule Chunks do
use GenServer

#-------------------------------------------------------------------
# GenServer Function Definitions

def start_link(opts \\ []) do
  GenServer.start_link(__MODULE__, :ok, opts)
end

def stop() do
  GenServer.call(:stop)
end

#-------------------------------------------------------------------

def init(:ok) do
  IO.puts("Chunks initializing")
  {:ok, HashDict.new()}
end

def terminate(_reason, _state) do
  IO.puts("Chunks terminating")
  :ok
end

#def code_change(_oldVsn, state, _extra) do
#  {:ok, state}
#end

def add_chunk(server, coords) do
  GenServer.cast(server, coords, {:add, coords})
end

def get_chunk(server, coords) do
  GenServer.call(server, {:get, coords, notify_pid})
end

#-------------------------------------------------------------------

def handle_cast({:add, coords}) do
  {:noreply, HashDict.new()}
end

def handle_call(:get, _from, %State{count: count}) do 
  {:reply, count, %State{count: count+1} }
end

#------------------------------------------------------------------

end
