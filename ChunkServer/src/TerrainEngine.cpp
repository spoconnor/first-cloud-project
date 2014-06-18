//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// TerrainEngine.cpp
//
// By Sean OConnor
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#include "ChunkServer.h"


#include "SettingsMgr.h"
#include "LogFile.h"
#include <iostream>
#include <stdlib.h>
    
using namespace TerrainEngine;

void InitEngine()
{
    engineThread = GThread::create("EngineThread", ThreadMain);

}

void StartEngine()
{
    engineThread->start();
}


void ThreadMain()
{ 
  SEANLIB::GlobalLogObject::OpenLog("output.txt");
  SEANLIB::GlobalLogObject::GetLog().SetLogLevel(eInfo);
  
/*
  GApp::Settings settings;
  SETTINGSMGR->Read("Settings.txt");
  settings.window.width = SETTINGSMGR->GetIntSetting("ScreenWidth", 640);
  settings.window.height = SETTINGSMGR->GetIntSetting("ScreenHeight", 480);
  settings.window.depthBits = SETTINGSMGR->GetIntSetting("ScreenDepth", 16);
  settings.window.resizable = SETTINGSMGR->GetBoolSetting("ScreenResizable", true);
  settings.window.fullScreen = SETTINGSMGR->GetBoolSetting("ScreenFull", false);
  settings.window.hardware = SETTINGSMGR->GetBoolSetting("ScreenUseHardware", false);
  
  settings.useDeveloperTools = false;
  //settings.useNetwork = SETTINGSMGR->GetBoolSetting("UseNetwork", false);
*/
  
  NORMALLOG << "[main] Ready to go" << endl;
  int result = Engine(settings).run();  
}




