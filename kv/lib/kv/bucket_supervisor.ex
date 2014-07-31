defmodule KV.Bucket.Supervisor do
  use Supervisor

  def start_link(opts \\ []) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

# fn to receive a supervisor and start a bucket process as a child of that supervisor
  def start_bucket(supervisor) do
    Supervisor.start_child(supervisor, [])
  end

  def init(:ok) do
    children = [
      worker(KV.Bucket, [], type: :temporary)
# temporary worker, wont be restarted
    ]

    supervise(children, strategy: :simple_one_for_one)
  end
end



#Erlang/OTP 17 [erts-6.1] [source] [64-bit] [async-threads:10] [kernel-poll:false]
#
#Compiled lib/kv.ex
#Compiled lib/kv/bucket_supervisor.ex
#Compiled lib/kv/supervisor.ex
#Compiled lib/kv/registry.ex
#Generated kv.app
#Interactive Elixir (0.14.2) - press Ctrl+C to exit (type h() ENTER for help)
#iex(1)> {:ok, sup} = KV.Bucket.Supervisor.start_link
#{:ok, #PID<0.89.0>}
#iex(2)> {:ok, bucket} = KV.Bucket.Supervisor.start_bucket(sup)
#{:ok, #PID<0.91.0>}
#iex(3)> KV.Bucket.put(bucket, "test", 3)
#:ok
#iex(4)> KV.Bucket.get(bucket, "test")
#3

