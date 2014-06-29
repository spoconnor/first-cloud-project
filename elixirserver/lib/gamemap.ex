defmodule GameMap do

  defp newRoom do
    spawn(fn -> roomContents([]) end)
  end

  defp roomContents(state) do
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
        roomContents(List.delete state, object)
      {sender, :getcontents} ->
        send(sender, {self(), state})
        roomContents(state)
    end
  end

  def createRooms( numRooms ) when is_integer(numRooms) do
    Enum.map 1..numRooms, fn(r) -> {r, newRoom} end
  end

  defp findRoom( rooms, room ) do  
   {_, pid} = List.keyfind( rooms, room, 0)
   pid
  end

  def addToRoom( rooms, room, object ) do
   IO.puts "Adding to #{room}"
   send(findRoom( rooms, room), { self(), :add, object} )
  end

  def addToRandomRoom( rooms, object ) do
   room = :random.uniform(Enum.count(rooms))
   addToRoom( rooms, room, object ) 
  end

  def examineRoom( rooms, room ) do
   send(findRoom( rooms, room), { self(), :examine} )
  end

  def getRoomContents( rooms, room ) do
    send(findRoom( rooms, room ), { self(), :getcontents})
    receive do
      {sender, state} -> state
    end
  end

  def removeFromRoom( rooms, room, object ) do
   send(findRoom( rooms, room), { self(), :del, object} )
  end

  def moveRooms( rooms, orig, dest, object ) do
   send(findRoom( rooms, orig), { self(), :del, object} )
   send(findRoom( rooms, dest), { self(), :add, object} )
  end

end
