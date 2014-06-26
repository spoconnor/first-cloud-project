defmodule Map do

  @numRooms 10

  def newRoom do
    spawn(fn -> roomContents([""]) end)
  end

  def roomContents(state) do
    receive do
      {sender, :examine} ->
        IO.puts "Contents:"
        Enum.each state, fn x -> IO.puts(x) end
        roomContents(state)
      {sender, :add, object} ->
        IO.puts "Adding #{object}"
        roomContents(state ++ [object])
      {sender, :del, object} ->
        IO.puts "Removing #{object}"
        roomContents(state)
    end
  end

  def createRooms do
    Enum.map 1..@numRooms, fn(_) -> newRoom end
  end
  

end
