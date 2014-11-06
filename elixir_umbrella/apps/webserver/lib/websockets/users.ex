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
    {header,data} = Packet.decode(payload)
    Lib.trace("MessageType:", header.msgtype)
#    actions(payload, msg)
    Enum.each users, fn {user, notify_pid} -> 
      # TODO - select target of message
      Lib.trace("Sending notify to #{user}")
      send notify_pid, payload
    end
    {:noreply, users}
  end

#  defp actions(%CommsMessages.Say{from: from, target: target, text: text}, users) do
#    Lib.trace("Action: say")
#    Lib.trace("#{msg.from}, #{msg.target}, #{msg.say}")
#  end

  defp actions(_unknown, users) do
    Lib.trace("Action: Unknown!")
  end
end

