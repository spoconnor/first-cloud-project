defmodule WorldState do
use GenServer

 def start_link(opts \\ []) do
  IO.puts "Starting Map link..."
  GenServer.start_link(__MODULE__, :ok, opts)
 end

 def init(:ok) do
  IO.puts("Starting Map worker...")

  {:ok, "State"}
 end

 def debug() do
  GenServer.call(:debug)
 end

 def stop() do
  GenServer.call(:stop)
 end

 def getState() do
  GenServer.call(:getState)
 end

 def resetState() do
  GenServer.call(:resetState)
 end

 def testCall(message) do
  GenServer.cast({:test, message})
 end

 # Server callbacks
 
 def handle_call(:stop, _from, state) do
  {:stop, :normal, :ok, state}
 end

 def handle_call({:test, message}) do
  IO.puts("Test #{message}")
 end

 def handle_call(:resetState, _from, state) do
  IO.puts("Reset State")
  {:reply, :ok, "state"}
 end
 def handle_call(:getState, _from, state) do
  IO.puts("Get State")
  {:reply, state, state}
 end

 def handle_cast({:test, message}, state) do
  IO.puts("Test #{message}")
 end

 def handle_info(_info, state) do
  IO.puts("Handle.info")
  {:noreply, state}
 end

 def terminate(_reason, state) do
  IO.puts("Terminate")
  :ok
 end

 def code_change(_oldVsn, state, _extra) do
  IO.puts("Code Change")
  {:ok, state}
 end

end
