defmodule Rabbit.Consumer do
 use Exrabbit.Subscriber

  def start_link() do
    :gen_server.start_link(Rabbit.Consumer,
                      [ queue: "testQ"],
                      #[host: "localhost",
                      # port: 5672,
                      # username: "guest",
                      # password: "guest",
                      # queue: "testQ"],
                      [])
  end

  def handle_message(msg, state) do
    case parse_message(msg) do
      nil -> IO.puts "Got nil message"
      {tag, payload} ->
        IO.puts "Got message with tag #{tag} and payload #{payload}"
        ack state[:channel], tag
    end
  end


#@behaviour :gen_bunny

#include_lib("eunit/include/eunit.hrl").
#export([ack_stuff/2, get_messages/1]).
#include("deps/gen_bunny/include/gen_bunny.hrl").

#defmodule State do
# defstruct(
#   messages: [], 
#   calls: [],
#   infos: [],
#   casts: [],
#   wspid: []
# )
#end

#def start_link(webSocketPid) do
#  [a,b] = tl(:string.tokens(:erlang.pid_to_list(self()),"<>."))
#  nameAsList = "consumer-" ++ a ++ "-" ++ b
#  nameAsAtom = :erlang.list_to_atom(nameAsList)
#  nameAsBin = :erlang.list_to_binary(nameAsList)
#  exchange =  :exchange.declare(exchange: <<"fanout">>, type: <<"fanout">>, durable: :true)
#  que = :queue.declare(queue: nameAsBin)
#  routingKey = <<"">>
#  connectInfo = {:network, "localhost", 5672, {<<"guest">>, <<"guest">>}, <<"/">>}
#  declareInfo = {exchange, que, routingKey}
#  :gen_bunny.start_link({:local, nameAsAtom}, __MODULE__, connectInfo, declareInfo, [webSocketPid])
#end

#def init([args]) do
#  IO.format("~p: init", [__MODULE__])
#  IO.format("Args: ~p~n", [args])
#  {:ok, %State{wspid: args}}
#end

#def init(_one,_two) do
#  IO.format("INIT TWO~n")
#  {:ok, %State{}}
#end

#def ack_stuff(pid, tag) do
#  :gen_bunny.cast(pid, {:ack_stuff, tag})
#end

#def get_messages(pid) do
#  :gen_bunny.call(pid, :get_messages)
#end
  
#def stop(pid) do
#  :gen_bunny.call(pid, :stop)
#end

#def handle_message(message, %State{messages: messages, wspid: wsPid}) do # TODO # when is_message?(message) or is_tagged_message?(message) do
#  {:amqp_msg, _rest , payload} = message
#  IO.format("| ~p going to send payload: ~p to ~p~n", [__MODULE__, payload, wsPid])
#  send wsPid,  {:rabbit,payload}
#  newMessages = [message|messages]
#  {:noreply, %State{messages: newMessages}}
#end

#def handle_message(message, state) do
#  IO.format("Got rubbish: ~p~n", [message])
#  {:norely, state}
#end

#def handle_call(:get_messages, _from, _state=%State{messages: messages}) do
#  IO.format("Getting messages~n", [])
#  {:reply, messages, %State{messages: []}}
#end

#def handle_cast(msg, %State{casts: casts}) do
#  {:noreply, %State{casts: [msg|casts]}}
#end

#def handle_info(info, %State{infos: infos}) do
#  IO.format("INFO~n")
#  {:noreply, %State{infos: [info|infos]}}
#end

#def terminate(reason, _state) do
#  IO.format("~p terminating with reason ~p~n", [__MODULE__, reason])
#  :ok
#end

end
