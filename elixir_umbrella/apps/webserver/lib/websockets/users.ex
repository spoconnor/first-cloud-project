defmodule Websocket.Users do
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def add_user(server, user, socket) do
    GenServer.cast(server, {:add, user, socket})
  end

  def notify_user(server, user, data) do
    GenServer.cast(server, {:notify, user, data})
  end

  

  def init(:ok) do
    Lib.trace("Starting Websocket.Users")
    users = HashDict.new
    {:ok, users}
  end

  def handle_cast({:add, user, socket}, users) do
    Lib.trace("Adding user #{user}")
    newUsers = HashDict.put(users, user, socket)
    {:noreply, newUsers}
  end

  def handle_cast({:notify, user, data}, users) do
    #todo
    Lib.trace("Notifying user")
    {:noreply, users}
  end

end

