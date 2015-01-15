#include "PerlinNoise.h"

enum eBlock
{
  Empty = 0,
  Rock = 1,
  Tree = 2,
  Wall = 3
};


class MapServer
{
public:
	MapServer();
	~MapServer();
	DumpMap();

private:
 	GenerateMap();

	uint _MapWidth;
	uint _MapHeight;
	uint _MapMinY;
	uint _MapMaxY;
	uint _OctaveCount;

	TArray2<eBlock> map;

	char sprites[4] = {' ', '#', 'o', '+'};
};
