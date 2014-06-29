defmodule KnowledgeItem do
  defstruct fact: {"Actor", "Action", "Target"},
    attributes: []
end

defmodule Knowledge do

  def addItem( knowledge, actor, action, target ) do
    addItem( knowledge, %KnowledgeItem{fact: {actor, action, target}} )
  end

  def addItem( knowledge, item ) do
    knowledge ++ [item]
  end

  def showall( knowledge ) do
    Enum.each knowledge, fn item -> 
      {a,b,c} = item.fact
      IO.puts("#{a}-#{b}-#{c}") end
  end

  def query( knowledge, question, actor, action ) do
    knowledge |> Enum.filter fn(item) ->
      {a,b,c} = item.fact
      a == actor && b == action
    end
      
  end

end
