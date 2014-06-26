#defmodule SimStats do
  #defstruct health: :random.uniform(10)
#end
#
#defmodule Sim do
  #defstruct desc: nil, location: {0,0}, stats: nil
#end
#
#defmodule SimDesc do
  #defstruct name: "", strength: :random.uniform(10), dexterity: :random.uniform(10), Charisma: :random.uniform(10)
#end
#
defmodule Server do

  @moduledoc """
  AI Sims Engine
  """

  def main(args) do
    args |> parse_args
    IO.puts "Started!"
  end

  defp parse_args(args) do
    options = OptionParse.parse(args, switches: [help: :boolean], aliases: [h: :help])
    case options do
      { [ help: true], _}  -> IO.puts "Help"
      { _, [ test1 ] }      -> IO.puts "test1 #{test1}"
      { _, [ test1, test2 ] }      -> IO.puts "test1 #{test1}, test2 #{test2}"
    end
  end

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
    #%Sim{}

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
        IO.puts "Received: '{#msg}' from '{#sender}' thanks!"
    end
  end

  
  

end


