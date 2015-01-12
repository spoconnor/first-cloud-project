#include "PerlinNoise.h"
using namespace std;

	PerlinNoise::PerlinNoise(unsigned long int seed)
	{
		//MTRand_int32 irand(init, length); // 32-bit int generator
		mRand(); // 32-bit double generator
		mRand.seed(seed);
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

	TArray2<float>* PerlinNoise::GenerateWhiteNoise(int width, int height)
	{
		TArray2<float>* noise = new TArray2<float>(width,height);
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				(*noise)(i,j) = mRand();
			}
		}
		return noise;
	}

	int PerlinNoise::Interpolate(int minY, int maxY, float t)
	{
		float u = 1 - t;
		return (int)(minY * u + maxY * t);
	}

	TArray2<int>* PerlinNoise::MapInts(int minY, int maxY, TArray2<float>* perlinNoise)
	{
		TArray2<int>* heightMap = new TArray2<int>(perlinNoise->rows(), perlinNoise->cols());
		for (unsigned int i = 0; i < perlinNoise->rows(); i++)
		{
			for (unsigned int j = 0; j < perlinNoise->cols(); j++)
			{
				(*heightMap)(i,j) = Interpolate(minY, maxY, (*perlinNoise)(i,j));
			}
		}
		return heightMap;
	}

	TArray2<float>* PerlinNoise::GenerateSmoothNoise(int octave, TArray2<float>* baseNoise)
	{
		TArray2<float>* smoothNoise = new TArray2<float>(baseNoise->rows(), baseNoise->cols());
		int samplePeriod = 1 << octave; // calculates 2 ^ k
		float sampleFrequency = 1.0f / samplePeriod;

		for (unsigned int i = 0; i < baseNoise->rows(); i++)
		{
			//calculate the horizontal sampling indices
			int iSample0 = (i / samplePeriod) * samplePeriod;
			int iSample1 = (iSample0 + samplePeriod) % baseNoise->rows(); //wrap around
			float horizontalBlend = (i - iSample0) * sampleFrequency;

			for (unsigned int j = 0; j < baseNoise->cols(); j++)
			{
				//calculate the vertical sampling indices
				int jSample0 = (j / samplePeriod) * samplePeriod;
				int jSample1 = (jSample0 + samplePeriod) % baseNoise->cols(); //wrap around
				float verticalBlend = (j - jSample0) * sampleFrequency;

				//blend the top two corners
				float top = Interpolate((*baseNoise)(iSample0,jSample0), (*baseNoise)(iSample1,jSample0), horizontalBlend);

				//blend the bottom two corners
				float bottom = Interpolate((*baseNoise)(iSample0,jSample1), (*baseNoise)(iSample1,jSample1), horizontalBlend);

				//final blend
				(*smoothNoise)(i,j) = Interpolate(top, bottom, verticalBlend);
			}
		}
		return smoothNoise;
	}

	TArray2<float>* PerlinNoise::GeneratePerlinNoise(int octaveCount, TArray2<float>* baseNoise)
	{
		TArray2<float>* smoothNoise[octaveCount];
		const float PERSISTANCE = 0.4f;

		//generate smooth noise
		for (int i = 0; i < octaveCount; i++)
		{
			smoothNoise[i] = GenerateSmoothNoise(i, baseNoise);
		}

		TArray2<float>* perlinNoise = new TArray2<float>(baseNoise->rows(),baseNoise->cols());

		float amplitude = 1.0f;
		float totalAmplitude = 0.0f;

		//blend noise together
		for (int octave = octaveCount - 1; octave >= 0; octave--)
		{
			amplitude *= PERSISTANCE;
			totalAmplitude += amplitude;

			for (unsigned int i = 0; i < baseNoise->rows(); i++)
			{
				for (unsigned int j = 0; j < baseNoise->cols(); j++)
				{
					(*perlinNoise)(i,j) += (*smoothNoise[octave])(i,j) * amplitude;
				}
			}
		}

		//normalisation
		for (unsigned int i = 0; i < baseNoise->rows(); i++)
		{
			for (unsigned int j = 0; j < baseNoise->cols(); j++)
			{
				(*perlinNoise)(i,j) /= totalAmplitude;
			}
		}
		return perlinNoise;
	}

	TArray2<int>* PerlinNoise::GetIntMap(int width, int height, int minY, int maxY, int octaveCount)
	{
		TArray2<float>* baseNoise = GenerateWhiteNoise(width, height);
		TArray2<float>* perlinNoise = GeneratePerlinNoise(octaveCount, baseNoise);
		return MapInts(minY, maxY, perlinNoise);
	}


