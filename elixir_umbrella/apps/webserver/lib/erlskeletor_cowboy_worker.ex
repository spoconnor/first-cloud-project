defmodule Erlskeletor_cowboy_worker do
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
  IO.puts "Starting worker..."
  {:ok, {}}
 end

 def handle_call(:stop, _from, state) do
  {:stop, :normal, :ok, state}
 end


end


