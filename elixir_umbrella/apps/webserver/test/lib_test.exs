defmodule LibTest do
  use ExUnit.Case

  # helper for chosing the index of a sibling value list
  test "Floor" do
    assert(Lib.floor(5) == 5)
    assert(Lib.floor(5.2) == 5)
    assert(Lib.floor(5.7) == 5)
    assert(Lib.floor(6.1) == 6)
  end

end
