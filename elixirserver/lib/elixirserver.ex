defmodule Elixirserver do

  @moduledoc """
  AI Sims Engine
  """

  defrecord SimDesc, name: "", strength: 0, dexterity: 0, Charisma: 0

  defrecord SimStats,
    health: 0

  defrecord Sim, 
    desc: nil, location: {0,0}, stats: nil

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
    :random.seed :erlang.now
    Sim.new(
      desc: SimDesc.new(
        name: "Fred",
        strength: randomStat,
        dexterity: randomStat,
        Charisma: randomStat
      ),
      location: randomLocation,
      stats: SimStats.new(
        health: 100
      ),
    )
  end
    
  def generateSims() do
    Enum.map 1..10, fn(_) -> generateSim() end
  end


  def hola do
    receive do
      {sender, msg} ->
        sender <- "Received: '{#msg}', thanks!"
    end
  end

  
  

end


