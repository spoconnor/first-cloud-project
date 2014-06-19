//============================================================================
// Name        : ChunkServer.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include "PolyVoxCore/CubicSurfaceExtractorWithNormals.h"
#include "PolyVoxCore/MarchingCubesSurfaceExtractor.h"
#include "PolyVoxCore/SurfaceMesh.h"
#include "PolyVoxCore/SimpleVolume.h"
#include <iostream>
#include <cstdint>

using namespace std;
using namespace PolyVox;



void createSphereInVolume(SimpleVolume<uint8_t>& volData, float fRadius)
{
    //This vector hold the position of the center of the volume
    Vector3DFloat v3dVolCenter(volData.getWidth() / 2, volData.getHeight() / 2, volData.getDepth() / 2);

    //This three-level for loop iterates over every voxel in the volume
    for (int z = 0; z < volData.getDepth(); z++)
    {
        for (int y = 0; y < volData.getHeight(); y++)
        {
            for (int x = 0; x < volData.getWidth(); x++)
            {
                //Store our current position as a vector...
                Vector3DFloat v3dCurrentPos(x,y,z);
                //And compute how far the current position is from the center of the volume
                float fDistToCenter = (v3dCurrentPos - v3dVolCenter).length();

                uint8_t uVoxelValue = 0;

                //If the current voxel is less than 'radius' units from the center then we make it solid.
                if(fDistToCenter <= fRadius)
                {
                    //Our new voxel value
                    uVoxelValue = 255;
                }

                //Wrte the voxel value into the volume
                volData.setVoxelAt(x, y, z, uVoxelValue);
            }
        }
    }
}


int main(int argc, char *argv[])
{
    cout << "!!!Hello World!!!" << endl; // prints !!!Hello World!!!

    //Create an empty volume and then place a sphere in it
    SimpleVolume<uint8_t> volData(PolyVox::Region(Vector3DInt32(0,0,0), Vector3DInt32(63, 63, 63)));
    createSphereInVolume(volData, 30);

    //A mesh object to hold the result of surface extraction
    SurfaceMesh<PositionMaterialNormal> mesh;

    //Create a surface extractor. Comment out one of the following two lines to decide which type gets created.
    CubicSurfaceExtractorWithNormals< SimpleVolume<uint8_t> > surfaceExtractor(&volData, volData.getEnclosingRegion(), &mesh);
    //MarchingCubesSurfaceExtractor< SimpleVolume<uint8_t> > surfaceExtractor(&volData, volData.getEnclosingRegion(), &mesh);

    //Execute the surface extractor.
    surfaceExtractor.execute();

    //Pass the surface to the OpenGL window
    //openGLWidget.setSurfaceMeshToRender(mesh);

    return 1;
}
