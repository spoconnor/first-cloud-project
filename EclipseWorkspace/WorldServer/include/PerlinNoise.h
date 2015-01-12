#include "TArray2.h"
#include "mtrand.h"

class PerlinNoise
{
public:
	PerlinNoise(unsigned long int seed);
	TArray2<int>* GetIntMap(int width, int height, int minY, int maxY, int octaveCount);
private:
	TArray2<float>* GenerateWhiteNoise(int width, int height);
	int Interpolate(int minY, int maxY, float t);
	TArray2<int>* MapInts(int minY, int maxY, TArray2<float>* perlinNoise);
	TArray2<float>* GenerateSmoothNoise(int octave, TArray2<float>* baseNoise);
	TArray2<float>* GeneratePerlinNoise(int octaveCount, TArray2<float>* baseNoise);

private:
	MTRand mRand;
};
