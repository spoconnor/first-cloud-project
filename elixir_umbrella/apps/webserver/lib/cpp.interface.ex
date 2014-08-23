defmodule Cpp.Interface do
use GenServer

# Client API

 def start_link(opts \\ []) do
  IO.puts "Starting worker link..."
  GenServer.start_link(__MODULE__, :ok, opts)
 end

 def stop() do
  GenServer.call(:stop)
 end

# Server Callbacks

 def init(:ok) do
  IO.puts "Starting c++ interface worker..."

  :erlang.load_nif("./testapp", 0)
  testapp.hello()
 end


end


