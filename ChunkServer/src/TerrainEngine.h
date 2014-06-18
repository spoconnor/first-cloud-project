/***************************************************************************
 *   Copyright (C) 2004 by Sean OConnor                                    *
 *   sc.oconnor@xtra.co.nz                                                 *
 *                                                                         *
 ***************************************************************************/

#ifndef _TERRAINENGINE_H_
#define _TERRAINENGINE_H_

#ifdef TERRAINENGINE
#define ComonDeclSpec __declspec(dllexport)
#else
#define ComonDeclSpec __declspec(dllimport)
#endif

#include "Global.h"
#include "Engine.h"

void ComonDeclSpec InitEngine();
void ComonDeclSpec StartEngine();
void ThreadMain();
G3D::GThreadRef engineThread;

    class ComonDeclSpec EngineInterface
    {
    public:
        //void InitEngine();
        //void StartEngine();

    private:
        
        

    };

#endif // _TERRAINENGINE_H_