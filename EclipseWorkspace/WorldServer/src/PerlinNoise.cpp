#include "PerlinNoise.h"
using namespace std;

	PerlinNoise::PerlinNoise(unsigned long seeds[])
	{
		//unsigned long init[4] = {0x123, 0x234, 0x345, 0x456}, length = 4;
		//MTRand_int32 irand(init, length); // 32-bit int generator
		mRand(seeds, 4); // 32-bit double generator
		// this is an example of initializing by an array
		// you may use MTRand(seed) with any 32bit integer
		// as a seed for a simpler initialization
		//MTRand drand; // double in [0, 1) generator, already init

		// generate the same numbers as in the original C test program
		//		std::printf("1000 32-bit integer random numbers:\n");
		//		for (int i = 0; i < 1000; ++i) {
		//			std::printf("%10lu ", irand());
		//			if ((i % 5) == 4) std::printf("\n");
		//		}
		//		std::printf("\n1000 random numbers in [0, 1):\n");
		//		for (int i = 0; i < 1000; ++i) {
		//			std::printf("%10.8f ", drand());
		//			if ((i % 5) == 4) std::printf("\n");
		//		}
	}

	PerlinNoise::~PerlinNoise()
	{
	}

	float[][] PerlinNoise::GenerateWhiteNoise(int width, int height)
	{
		float noise[width][height];
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				noise[i][j] = mRand();
			}
		}
		return noise;
	}

	int PerlinNoise::Interpolate(int minY, int maxY, float t)
	{
		float u = 1 - t;
		return (int)(minY * u + maxY * t);
	}

	int[][] PerlinNoise::MapInts(int minY, int maxY, float perlinNoise[][], int width, int height)
	{
		//int* heightMap = new int[width][height];
		std::array<std::array<int, height>, width> heightMap;

		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				heightMap[i][j] = Interpolate(minY, maxY, perlinNoise[i][j]);
			}
		}
		return heightMap;
	}

	float[][] PerlinNoise::GenerateSmoothNoise(int octave, float baseNoise[][], int width, int height)
	{
		float* smoothNoise = new float[width][height];
		int samplePeriod = 1 << octave; // calculates 2 ^ k
		float sampleFrequency = 1.0f / samplePeriod;

		for (int i = 0; i < width; i++)
		{
			//calculate the horizontal sampling indices
			int iSample0 = (i / samplePeriod) * samplePeriod;
			int iSample1 = (iSample0 + samplePeriod) % width; //wrap around
			float horizontalBlend = (i - iSample0) * sampleFrequency;

			for (int j = 0; j < height; j++)
			{
				//calculate the vertical sampling indices
				int jSample0 = (j / samplePeriod) * samplePeriod;
				int jSample1 = (jSample0 + samplePeriod) % height; //wrap around
				float verticalBlend = (j - jSample0) * sampleFrequency;

				//blend the top two corners
				float top = Interpolate(baseNoise[iSample0][jSample0], baseNoise[iSample1][jSample0], horizontalBlend);

				//blend the bottom two corners
				float bottom = Interpolate(baseNoise[iSample0][jSample1], baseNoise[iSample1][jSample1], horizontalBlend);

				//final blend
				smoothNoise[i][j] = Interpolate(top, bottom, verticalBlend);
			}
		}
		return smoothNoise;
	}

	float[][] PerlinNoise::GeneratePerlinNoise(int octaveCount, float baseNoise[][], int width, int height)
	{
		float* smoothNoise = new float[octaveCount][][]; //an array of 2D arrays containing

		const float PERSISTANCE = 0.4f;

		//generate smooth noise
		for (int i = 0; i < octaveCount; i++)
		{
			smoothNoise[i] = GenerateSmoothNoise(i, baseNoise, width, height);
		}

		float* perlinNoise = new float[width][height]; //an array of floats initialised to 0

		float amplitude = 1.0f;
		float totalAmplitude = 0.0f;

		//blend noise together
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

		//normalisation
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				perlinNoise[i][j] /= totalAmplitude;
			}
		}
		return perlinNoise;
	}

	int[][] PerlinNoise::GetIntMap(int width, int height, int minY, int maxY, int octaveCount)
	{
		float* baseNoise = GenerateWhiteNoise(width, height);
		float* perlinNoise = GeneratePerlinNoise(octaveCount, baseNoise, width, height);
		return MapInts(minY, maxY, perlinNoise, width, height);
	}

