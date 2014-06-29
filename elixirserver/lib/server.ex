defmodule Server do

  @moduledoc """
  AI Main Server
  """

  @numRooms 5
  @numSims 5

  def main(args) do
    #args |> parse_args
    IO.puts "Started!"
    rooms = GameMap.createRooms(@numRooms)
    sims = Sims.generateSims(@numSims)
    Enum.map( sims, fn(sim) -> GameMap.addToRandomRoom(rooms, sim) end )
  end

  def dosomething(rooms, sims) when is_list(sims) do
    Enum.each sims, fn(sim) -> dosomething(rooms, sim) end
  end

  def dosomething(rooms, sim) when is_map(sim) do
    IO.puts "Go: #{sim.desc.name}"
  end

  def hello do
    IO.puts "Hello world!"
  end

end


