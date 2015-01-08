defmodule WorldTest do
  use ExUnit.Case

  test "the truth" do
    assert 1 + 1 == 2
  end

  test "interpolate" do
    assert PerlinNoise.interpolate(5, 10, 0.0) == 5
    assert PerlinNoise.interpolate(5, 10, 0.5) == 7.5
    assert PerlinNoise.interpolate(5, 10, 1.0) == 10
  end

  test "whitenoise" do
    PerlinNoise.seedRandom(12345)
    PerlinNoise.generateWhiteNoise(3,3)
  end

end
