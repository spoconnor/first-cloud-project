defmodule Array2D do

  def new(width, height, default) do
    :array.new([{:size,height}, {:fixed,:true}, {:default, 
      :array.new([{:size,width}, {:fixed,:true}, {:default,default}])}])
  end

  def get(array, x, y), do: :array.get(x, :array.get(y, array))

  # Warning. Array is immutable. a new array is returned.
  def set(array, x, y, value) do
    line = :array.get(x, array)
    :array.set(x, :array.set(y, value, line), array)
  end

  def map(array, func) do
    :array.map(array, fn (i) ->
      :array.map(i, func.()) end)
  end

end
