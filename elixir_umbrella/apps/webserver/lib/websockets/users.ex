defmodule Websocket.Users do
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def add_user(server, user, notify_pid) do
    GenServer.cast(server, {:add, user, notify_pid})
  end

  def notify(server, payload) do
    GenServer.cast(server, {:notify, payload})
  end

  def init(:ok) do
    Lib.trace("Starting Websocket.Users")
    users = HashDict.new
    {:ok, users}
  end

  def handle_cast({:add, user, notify_pid}, users) do
    Lib.trace("Adding user #{user}")
    newUsers = HashDict.put(users, user, notify_pid)
    {:noreply, newUsers}
  end

  def handle_cast({:notify, payload}, users) do
    #todo
    Lib.trace("Notifying users", payload)
temp = CommsMessages.Base.new(msgtype: :'ESay', say: CommsMessages.Base.Say.new(from: 0, target: 999, text: "hello"))
temp2 = CommsMessages.Base.encode(temp)
Lib.trace("Test msg:", temp2)
temp3 = CommsMessages.Base.decode(temp2)
Lib.trace("Test ok")
    msg = CommsMessages.Base.decode(payload)
    Lib.trace("MessageType:", msg.msgtype)
#    actions(payload, msg)
    Enum.each users, fn {user, notify_pid} -> 
      # TODO - select target of message
      IO.puts("Sending notify to #{user}")
      send notify_pid, payload
    end
    {:noreply, users}
  end

  defp actions(%CommsMessages.Base{msgtype: :'ESay', say: msg}, users) do
    Lib.trace("Action: say")
    Lib.trace("#{msg.from}, #{msg.target}, #{msg.say}")
  end

  defp actions(_unknown, users) do
    Lib.trace("Action: Unknown!")
  end
end

