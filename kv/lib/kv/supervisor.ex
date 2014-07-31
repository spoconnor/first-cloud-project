defmodule KV.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end

  @manager_name KV.EventManager
  @registry_name KV.Registry

  def init(:ok) do
    children = [
      worker(GenEvent, [[name: @manager_name]]),
      supervisor(KV.Bucket.Supervisor, [[name: @bucket_sup_name]]),
      worker(KV.Registry, [@manager_name, [name: @registry_name]])
    ]

    supervise(children, strategy: :one_for_one)
  # one_for_one = if one child dies, replace it with another
  end
end


#Interactive Elixir (0.14.2) - press Ctrl+C to exit (type h() ENTER for help)
#iex(1)> KV.Supervisor.start_link
#{:ok, #PID<0.66.0>}
#iex(2)> KV.Registry.create(KV.Registry, "test")
#:ok
#iex(3)> KV.Registry.lookup(KV.Registry, "test")
#{:ok, #PID<0.72.0>}

