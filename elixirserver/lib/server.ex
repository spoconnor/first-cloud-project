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

  def hello do
    IO.puts "Hello world!"
  end

end


