defmodule PerlinNoise do
use Bitwise

  # returns int
  # int minY, int maxY, float t
  defp interpolate(minY, maxY, t) do
    u = 1 - t
    minY * u + maxY * t
  end

  # Return a width*height list of random floats between 0 and 1
  defp GenerateWhiteNoise(width, height) do
    #Stream.repeatedly(fn -> :random.uniform end) |> Enum.take(width * height)
    :array.new([{:size,height}, {:fixed,:true}, {:default, 
      :array.new([{:size,width}, {:fixed,:true}, {:default, :random.uniform}]))
  end

  # Returns int[][]
  # int minY, int maxY, float[][] perlinNoise
  defp MapInts(minY, maxY, perlinNoise) do
    #width = perlinNoise.Length;
    #height = perlinNoise[0].Length;
    #int[][] heightMap = GetEmptyArray<int>(width, height);
    #for (int i = 0; i < width; i++)
    #  for (int j = 0; j < height; j++)
    #    heightMap[i][j] = Interpolate(minY, maxY, perlinNoise[i][j]);
    #}
    #return heightMap;
    Enum.map(perlinNoise, fn {i} -> Interpolate(minY, maxY, i) end)
  end

  defp lookup(array, x, y) do
    :array.get(x, :array.get(y, array))
  end

  # returns float[][]
  # float[][] baseNoise, int octave
  defp GenerateSmoothNoise(width, height, baseNoise, octave) do
    samplePeriod = 1 <<< octave  # calculates 2 ^ k
    sampleFrequency = 1.0 / samplePeriod

    smoothNoise = :array.new([{:size,height}, {:fixed,:true}, {:default, 
      :array.new([{:size,width}, {:fixed,:true}, {:default, 0}]))

    #for (int i = 0; i < width; i++)
    Enum.each(list.seq(0, width-1), 
    fn (i) ->
      # calculate the horizontal sampling indices
      iSample0 = div(i, samplePeriod) * samplePeriod
      iSample1 = rem( (iSample0 + samplePeriod), width) #wrap around
      horizontalBlend = (i - iSample0) * sampleFrequency

      Enum.each(list.seq(0, height-1), 
      fn (j) ->
        # calculate the vertical sampling indices
        jSample0 = div(j, samplePeriod) * samplePeriod
        jSample1 = rem( (jSample0 + samplePeriod), height) #wrap around
        verticalBlend = (j - jSample0) * sampleFrequency

        # blend the top two corners
        top = Interpolate(
          lookup(baseNoise,iSample0,jSample0), 
          lookup(baseNoise,iSample1,jSample0), 
          horizontalBlend)

        # blend the bottom two corners
        bottom = Interpolate(
          lookup(baseNoise,iSample0,jSample1), 
          lookup(baseNoise,iSample1,jSample1), 
          horizontalBlend)

        # final blend
        smoothNoise[i][j] = Interpolate(top, bottom, verticalBlend)
      )
    )
    return smoothNoise
  end


  defp Blend([p1|perlinNoise], [o1|octaveNoise], amplitude, result) do
    Blend(perlinNoise, octaveNoise, amplitude, result ++ [p1 + o1 * amplitude])
  end
  defp Blend([], [], amplitude, result) do
    result
  end

  # returns float[][] 
  # float[][] baseNoise, int octaveCount
  defp GeneratePerlinNoise(width, height, baseNoise, octaveCount) do
    smoothNoise = new float[octaveCount][][]; #an array of 2D arrays containing
    PERSISTANCE = 0.4f

    # generate smooth noise
    octaves = list.seq(octaveCount-1, 0, -1)
    smoothNoise = Enum.map(octaves, fn (octave) -> GenerateSmoothNoise(width,height, baseNoise, octave) end)

    perlinNoise = GetEmptyArray(width, height) #an array of floats initialised to 0

    amplitude = 1f
    totalAmplitude = 0.0f

    # blend noise together
    Enum.map(smoothNoise, fn (octaveNoise) -> 
      amplitude = amplitude * PERSISTANCE
      totalAmplitude = totalAmplitude + amplitude
      perlinNoise = Blend(perlinNoise, octaveNoise, amplitude, [])
    end)

    # normalisation
    normalizedPerlinNoise = Enum.map(perlinNoise, fn {i} -> i / totalAmplitude end)
  end

  #--------------------------------------------------------------------------

  def SeedRandom() do
    :random.seed(:erlang.now)
  end

  # returns int[][] 
  # int width, int height, int minY, int maxY, int octaveCount
  def GetIntMap(width, height, minY, maxY, octaveCount) do
    baseNoise = GenerateWhiteNoise(width, height)
    perlinNoise = GeneratePerlinNoise(width, height, baseNoise, octaveCount)
    MapInts(minY, maxY, perlinNoise)
  end

  #--------------------------------------------------------------------------
end
