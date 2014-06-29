defmodule Sim do
  defstruct name: "",
    knowledge: []
end

defmodule Sims do

  defp generateSim() do
    sim = %Sim{name: generateName(),
      knowledge: [] |>
        Knowledge.addItem( "sim", "strength", :random.uniform(10) ) |>
        Knowledge.addItem( "sim", "charisma", :random.uniform(10) ) |>
        Knowledge.addItem( "sim", "dexterity", :random.uniform(10) ) |>
        Knowledge.addItem( "sim", "health", :random.uniform(10) ) }
  end

  defp rndchr() do
    :random.uniform(26)+65
  end

  defp generateName() do
    <<rndchr, rndchr, rndchr, rndchr, rndchr>>
  end

  def generateSims(numSims) do
    :random.seed :erlang.now
    Enum.map 1..numSims, fn(_) -> generateSim() end
  end

  def querySim(sim, question, actor, action) do
    Knowledge.query( sim.knowledge, question, actor, action)
  end

end


