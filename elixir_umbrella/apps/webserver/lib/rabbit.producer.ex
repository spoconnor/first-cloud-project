defmodule Rabbit.Producer do

#include_lib("./deps/gen_bunny/include/gen_bunny.hrl").

use GenServer

defmodule ExchangeDeclare do
  require Record
  defstruct(
    Record.extract(:'exchange.declare', from_lib: "rabbit_common/include/rabbit_framing.hrl")
  )
end

#-define(SERVER, ?MODULE). 
defmodule State do
  defstruct(
    pid: nil
  )
end

def start_link() do
  :gen_server.start_link({:local, :rabbit}, __MODULE__, [], [])
end

def init([]) do
  exchange = %ExchangeDeclare{exchange: <<"fanout">>, type: <<"fanout">>, durable: :true}
  declareInfo = {exchange}
  {:ok, pid} = :bunnyc.start_link(:mq_producer,
    {:network, "localhost", 5672, {<<"guest">>, <<"guest">>}, <<"/">>},
    declareInfo,
    [] )
  {:ok, %State{pid: pid}}
end

def handle_call({:send_message, msg}, _from, state) when is_list(msg) do
  binMsg = :erlang.list_to_binary(msg)
  :bunnyc.publish(:mq_producer, <<"myqueue">>, binMsg)
  reply = :ok
  {:reply, reply, state}
end

def handle_call({:send_message, msg}, _from, state) when is_binary(msg) do
  :bunnyc.publish(:mq_producer, <<"myqueue">>, msg)
  reply = :ok
  {:reply, reply, state}
end

def handle_call(request, _from, state) do
  IO.format("Producer Request is: ~p~n", [request])
  {:reply, :ok, state}
end

def handle_cast(_msg, state) do
  {:noreply, state}
end

def handle_info(_info, state) do
  {:noreply, state}
end

def terminate(_reason, _state) do
  :ok
end

def code_change(_oldVsn, state, _extra) do
  {:ok, state}
end

#===================================================================

def send_message(msg) do
  :gen_server.call(__MODULE__ , {:send_message, msg })
end

#===================================================================

end
