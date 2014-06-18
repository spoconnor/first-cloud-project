//////////////////////////////////////////////////////
// SettingsMgr.h
//////////////////////////////////////////////////////

#ifndef _SETTINGSMGR_H
#define _SETTINGSMGR_H

#include <map>
#include <fstream>
#include <string>

#ifdef TERRAINENGINE
#define ComonDeclSpec __declspec(dllexport)
#else
#define ComonDeclSpec __declspec(dllimport)
#endif

using namespace std;
namespace SEANLIB {

#define SETTINGSMGR SettingsMgr::GetSettingsMgr()
	
class ComonDeclSpec SettingsMgr
{
protected:

  SettingsMgr();
  ~SettingsMgr();

public:

  static SettingsMgr* GetSettingsMgr();
  
  void Read (const string& filename);
  void Write(const string& filename);

  int   GetIntSetting(const string& name, int alternate=0);
  float GetFloatSetting(const string& name, float alternate=0.0f);
  bool  GetBoolSetting(const string& name, bool alternate=false);
  string GetStrSetting(const string& name, const string& alternate="");

  void SetIntSetting(const string& name, const int value);
  void SetFloatSetting(const string& name, const float value);
  void SetBoolSetting(const string& name, const bool value);
  void SetStrSetting(const string& name, const string& value);

private:
  static SettingsMgr* instance;
  string			  mFileName;
  map<string, string> mStrSettings;
  map<string, int>    mIntSettings;
  map<string, float>  mRealSettings;

};

} // namespace
#endif // _SETTINGSMGR_H
