defmodule State do
  require Record
  Record.defrecord(:state,
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
  require Record
  Record.defrecord(:user,
    x:  "0",
    y:  "0",
    id:  0,
    user:  "",
    map:  0,
    lastMessage:  :u.munixtime()-3000,
    lastAction:  :u.munixtime()-3000,
    floodTest:  [ 4000,4000,4000,4000,4000,4000 ], # How to do this?
    sprite:  "0",
    ip:  nil,
    auth:  "0",
    sock:  nil,
    pid:  nil
  )
end

defmodule Simple do
  require Record
  Record.defrecord(:simple,
    id:  0,
    map:  0,
    sock:  nil
  )
end

