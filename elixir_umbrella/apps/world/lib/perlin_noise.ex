defmodule PerlinNoise do

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
    width = perlinNoise.Length;
    height = perlinNoise[0].Length;
    int[][] heightMap = GetEmptyArray<int>(width, height);

    for (int i = 0; i < width; i++)
    {
      for (int j = 0; j < height; j++)
      {
        heightMap[i][j] = Interpolate(minY, maxY, perlinNoise[i][j]);
      }
    }
    return heightMap;
  end

  # returns T[][]
  # int width, int height
  defp GetEmptyArray(width, height) do
    var image = new T[width][];

    for (int i = 0; i < width; i++)
    {
      image[i] = new T[height];
    }
    return image;
  end

  # returns float[][]
  # float[][] baseNoise, int octave
  defp GenerateSmoothNoise(baseNoise, octave) do
    width = baseNoise.Length;
    height = baseNoise[0].Length;

    float[][] smoothNoise = GetEmptyArray<float>(width, height);
    int samplePeriod = 1 << octave; // calculates 2 ^ k
    float sampleFrequency = 1.0f / samplePeriod;

    for (int i = 0; i < width; i++)
    {
      # calculate the horizontal sampling indices
      int iSample0 = (i / samplePeriod) * samplePeriod;
      int iSample1 = (iSample0 + samplePeriod) % width; //wrap around
      float horizontalBlend = (i - iSample0) * sampleFrequency;

      for (int j = 0; j < height; j++)
      {
        # calculate the vertical sampling indices
        int jSample0 = (j / samplePeriod) * samplePeriod;
        int jSample1 = (jSample0 + samplePeriod) % height; //wrap around
        float verticalBlend = (j - jSample0) * sampleFrequency;

        # blend the top two corners
        float top = Interpolate(baseNoise[iSample0][jSample0], baseNoise[iSample1][jSample0], horizontalBlend);

        # blend the bottom two corners
        float bottom = Interpolate(baseNoise[iSample0][jSample1], baseNoise[iSample1][jSample1], horizontalBlend);

        # final blend
        smoothNoise[i][j] = Interpolate(top, bottom, verticalBlend);
      }
    }
    return smoothNoise;
  end

  # returns float[][] 
  # float[][] baseNoise, int octaveCount
  defp GeneratePerlinNoise(baseNoise, octaveCount) do
    width = baseNoise.Length;
    height = baseNoise[0].Length;

    var smoothNoise = new float[octaveCount][][]; //an array of 2D arrays containing

    const float PERSISTANCE = 0.4f;

    # generate smooth noise
    for (int i = 0; i < octaveCount; i++)
    {
      smoothNoise[i] = GenerateSmoothNoise(baseNoise, i);
    }

    float[][] perlinNoise = GetEmptyArray<float>(width, height); //an array of floats initialised to 0

    float amplitude = 1f;
    float totalAmplitude = 0.0f;

    # blend noise together
    for (int octave = octaveCount - 1; octave >= 0; octave--)
    {
      amplitude *= PERSISTANCE;
      totalAmplitude += amplitude;

      for (int i = 0; i < width; i++)
      {
        for (int j = 0; j < height; j++)
        {
          perlinNoise[i][j] += smoothNoise[octave][i][j] * amplitude;
        }
      }
    }

    # normalisation
    for (int i = 0; i < width; i++)
    {
      for (int j = 0; j < height; j++)
      {
        perlinNoise[i][j] /= totalAmplitude;
      }
    }
    return perlinNoise;
  end

  #--------------------------------------------------------------------------

  def SeedRandom() do
    :random.seed(:erlang.now)
  end

  # returns int[][] 
  # int width, int height, int minY, int maxY, int octaveCount
  def GetIntMap(width, height, minY, maxY, octaveCount) do
    baseNoise = GenerateWhiteNoise(width, height);
    perlinNoise = GeneratePerlinNoise(baseNoise, octaveCount);
    return MapInts(minY, maxY, perlinNoise);
  end

  #--------------------------------------------------------------------------
end
