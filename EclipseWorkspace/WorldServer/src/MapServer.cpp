#include "MapServer.h"
#include <iostream>
#include <stdlib.h>

MapServer::MapServer() :
	_MapWidth(40),
	_MapHeight(40),
	_MapMinY(0),
	_MapMaxY(1),
	_OctaveCount(3)
{
}

MapServer::~MapServer()
{
}

void MapServer::GenerateMap()
{
  PerlinNoise perlin(1234567);
  map = perlin.GetIntMap(_MapWidth, _MapHeight, _MapMinY, _MapMaxY, _OctaveCount);
 
}

void MapServer::DumpMap()
{
  for (int i=0; i<_MapHeight; i++)
  {
    for (int j=0; j<_MapWidth; j++)
    {
      std::cout << sprites[map(i,j)];
    }
    std::cout << std::endl;
  }
}
