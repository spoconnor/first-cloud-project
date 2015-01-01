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
    Stream.repeatedly(fn -> :random.uniform end) |> Enum.take(width * height)
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

  # returns empty T[][]
  # int width, int height
  defp GetEmptyArray(width, height) do
    Stream.repeatedly(fn -> 0 end) |> Enum.take(width * height)
  end

  defp lookup(list, _width, height, x, y) do
    :lists.nth(height * x + y, list)
  end

  # returns float[][]
  # float[][] baseNoise, int octave
  defp GenerateSmoothNoise(width, height, baseNoise, octave) do
    smoothNoise = GetEmptyArray(width, height)
    samplePeriod = 1 <<< octave  # calculates 2 ^ k
    sampleFrequency = 1.0 / samplePeriod

    for (int i = 0; i < width; i++)
    {
      # calculate the horizontal sampling indices
      iSample0 = div(i, samplePeriod) * samplePeriod
      iSample1 = rem( (iSample0 + samplePeriod), width) #wrap around
      horizontalBlend = (i - iSample0) * sampleFrequency

      for (int j = 0; j < height; j++)
      {
        # calculate the vertical sampling indices
        jSample0 = div(j, samplePeriod) * samplePeriod
        jSample1 = rem( (jSample0 + samplePeriod), height) #wrap around
        verticalBlend = (j - jSample0) * sampleFrequency

        # blend the top two corners
        top = Interpolate(
          lookup(baseNoise,width,height,iSample0,jSample0), 
          lookup(baseNoise,width,height,iSample1,jSample0), 
          horizontalBlend)

        # blend the bottom two corners
        bottom = Interpolate(
          lookup(baseNoise, width,height,iSample0,jSample1), 
          lookup(baseNoise, width,height,iSample1,jSample1), 
          horizontalBlend)

        # final blend
        smoothNoise[i][j] = Interpolate(top, bottom, verticalBlend)
      }
    }
    return smoothNoise
  end

  # returns float[][] 
  # float[][] baseNoise, int octaveCount
  defp GeneratePerlinNoise(width, height, baseNoise, octaveCount) do
    smoothNoise = new float[octaveCount][][]; #an array of 2D arrays containing
    PERSISTANCE = 0.4f

    # generate smooth noise
    for (int i = 0; i < octaveCount; i++)
    {
      smoothNoise[i] = GenerateSmoothNoise(width,height,baseNoise, i)
    }

    perlinNoise = GetEmptyArray(width, height) #an array of floats initialised to 0

    amplitude = 1f
    totalAmplitude = 0.0f

    # blend noise together
    for (int octave = octaveCount - 1; octave >= 0; octave--)
    {
      amplitude = amplitude * PERSISTANCE
      totalAmplitude = totalAmplitude + amplitude

      for (int i = 0; i < width; i++)
      {
        for (int j = 0; j < height; j++)
        {
          perlinNoise[i][j] = perlinNoise[i][j] + smoothNoise[octave][i][j] * amplitude
        }
      }
    }

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
