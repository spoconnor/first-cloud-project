defmodule Hello_World_Sup do
  @behaviour :supervisor
 
  #-spec start_link() do
  #  {ok, pid()}
  #end

  def start_link() do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  # supervisor

  def init([]) do
    Procs = []
    {:ok, {{:one_for_one, 10, 10}, Procs}}
  end

end
