defmodule HelloServer do
use GenServer

defmodule State do
 defstruct(
   count: 0
 )
end

#---------------------------------------------------------------------------
# API Function Definitions

# spawn and links to new process in one atomic step
def start_link(opts \\ []) do
  GenServer.start_link( __MODULE__, :ok, opts)
end

# Use cast to send async message
def stop() do
  GenServer.cast(:stop)
end

# Call fn, don't expect a response
def say_hello() do
  GenServer.cast( :say_hello)
end

# Call, but do want a response
def get_count() do
  GenServer.call( :get_count)
end

#---------------------------------------------------------------------------
# GenServer Function Definitions

# Called in response to GenServer.start_link/4. Initialize state
def init(:ok) do
  {:ok, %State{count: 0}}
end

# synchronously response with count, and update state
def handle_call(:get_count, _from, %State{count: count}) do 
  {:reply, count, %State{count: count+1} }
end

# deal with Stop request
def handle_cast(:stop, state) do
  {:stop, :normal, state }
end

# async call, with no reply
def handle_cast(:say_hello, state) do
  IO.format("Hello~n")
  {:noreply, %State{count: state.count+1} }
end

# out-of-band msgs
def handle_info(info, state) do
  error_logger.info_msg("~p~n", [info])
  {:noreply, state}
end

# invoked by GenServer
def terminate(_reason, _state) do
  error_logger.info_msg("terminating~n")
  :ok
end

# Called during release up/down-grade to update internal state
def code_change(_oldVsn, state, _extra) do
  {:ok, state}
end

#------------------------------------------------------------------

end
