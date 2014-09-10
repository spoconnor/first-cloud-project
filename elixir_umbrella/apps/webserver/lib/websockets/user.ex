defmodule State do
  defstruct(
    maps:  :array.new(2,{:default,:dict.new()}),
    increment:  0,
    lookupByID:  :dict.new(),
    lookupByName:  :gb_trees.empty(),
    lookupByIP:  :gb_trees.empty(),
    banned:  [],
    sock:  nil
  )
end

defmodule User do
  defstruct( 
    x:  "0",
    y:  "0",
    id:  0,
    user:  "",
    map:  0,
    lastMessage:  Lib.munixtime()-3000,
    lastAction:  Lib.munixtime()-3000,
    floodTest:  [ for x <- 1..6 do 4000 end ], 
    sprite:  "0",
    ip:  nil,
    auth:  "0",
    sock:  nil,
    pid:  nil
  )
end

defmodule Simple do
  defstruct(
    id:  0,
    map:  0,
    sock:  nil
  )
end

