defmodule Rabbit.Producer do

  def sendMsg(msg) do
    {:ok, conn} = AMQP.Connection.open
    {:ok, chan} = AMQP.Channel.open(conn)
    AMQP.Basic.publish chan, "test_exchange", "", msg
  end

end

