defmodule PhoenixWebserver.Supervisor do
  use Supervisor

  def start_link do
    IO.puts "Supervisor.start_link"
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    IO.puts "Supervisor.init"
    children = []

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end
end
