include "Array2dInt.h"

class PerlinNoise
{
public:
	PerlinNoise(unsigned long seeds[]);
	Array2dInt GetIntMap(int width, int height, int minY, int maxY, int octaveCount);
private:
	float[][] GenerateWhiteNoise(int width, int height);
	int Interpolate(int minY, int maxY, float t);
	Array2dInt MapInts(int minY, int maxY, float perlinNoise[][], int width, int height);
	float[][] GenerateSmoothNoise(int octave, float baseNoise[][], int width, int height);
	float[][] GeneratePerlinNoise(int octaveCount, float baseNoise[][], int width, int height);

private:
	MTRand mRand;
};
