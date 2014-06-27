defmodule SimStats do
  defstruct health: :random.uniform(10)
end

defmodule SimDesc do
  defstruct name: "", strength: :random.uniform(10), dexterity: :random.uniform(10), Charisma: :random.uniform(10)
end

defmodule Sim do
  defstruct desc: %SimDesc{}, stats: %SimStats{}
end

defmodule Sims do

  defp generateSim() do
    %Sim{}
  end

  def generateSims(numSims) do
    :random.seed :erlang.now
    Enum.map 1..numSims, fn(_) -> generateSim() end
  end

end


