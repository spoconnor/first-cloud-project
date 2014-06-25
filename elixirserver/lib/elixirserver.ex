defmodule SimStats do
  defstruct health: :random.uniform(10)
end

defmodule Sim do
  defstruct desc: nil, location: {0,0}, stats: nil
end

defmodule SimDesc do
  defstruct name: "", strength: :random.uniform(10), dexterity: :random.uniform(10), Charisma: :random.uniform(10)
end

defmodule Elixirserver do

  @moduledoc """
  AI Sims Engine
  """

  def hello do
    IO.puts "Hello world!"
  end

  def randomLocation() do
    { :random.uniform(10), :random.uniform(10) }
  end

  def randomStat() do
    :random.uniform(10)
  end

  def generateSim() do
    %Sim{}
    #Sim.new(
    #  desc: SimDesc.new(
    #    name: "Fred",
    #    strength: randomStat,
    #    dexterity: randomStat,
    #    Charisma: randomStat
    #  ),
    #  location: randomLocation,
    #  stats: SimStats.new(
    #    health: 100
    #  ),
    #)
  end
    
  def generateSims() do
    :random.seed :erlang.now
    Enum.map 1..10, fn(_) -> generateSim() end
  end


  def hola do
    receive do
      {sender, msg} ->
        IO.puts "Received: '{#msg}', thanks!"
    end
  end

  
  

end


