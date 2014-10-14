defmodule HelloServerTest do
  use ExUnit.Case

  test "the truth" do
    assert 1 + 1 == 2
  end

  test "start link" do
    {:ok, pid} = HelloServer.start_link
    :ok = HelloServer.stop(pid)
  end

  test "say hello" do
    {:ok, pid} = HelloServer.start_link
    :ok = HelloServer.say_hello(pid) # async call
    :ok = HelloServer.stop(pid)
  end

  test "Count" do
    {:ok, pid} = HelloServer.start_link
    0 = HelloServer.get_count(pid)
    1 = HelloServer.get_count(pid)
    2 = HelloServer.get_count(pid)
    :ok = HelloServer.stop(pid)
  end

end

