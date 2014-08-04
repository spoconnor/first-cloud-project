defmodule WebServer.Supervisor do
  use Supervisor

  def start_link do
    IO.puts "Supervisor.start_link"
    :supervisor.start_link(__MODULE__,[])
  end

  def init([]) do
    IO.puts "Supervisor.init"

    #children = []
    children = [
      worker(WebServer.Worker, [])
    ]

    Supervisor.start_link(children, :one_for_one)
  end
end
