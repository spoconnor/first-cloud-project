///////////////////////////////////////////////////////
// SettingsMgr.cpp
///////////////////////////////////////////////////////

#include "SettingsMgr.h"
#include "LogFile.h"
#include "StrTools.h"
#include "Token.h"

using namespace std;

namespace SEANLIB {

SettingsMgr* SettingsMgr::instance = NULL;
	
SettingsMgr::SettingsMgr()
{
}

SettingsMgr::~SettingsMgr()
{
	if (NULL != mFileName)
	{
		Write(mFileName);
	}
}

/*static*/SettingsMgr* SettingsMgr::GetSettingsMgr()
{
  if (instance == NULL)
    instance = new SettingsMgr();
  return instance;
}

void SettingsMgr::Read(const string& filename)
{
  mFileName = filename;
  ifstream fin;
  fin.open(filename.c_str());
  if (!fin)
  {
    ERRORLOG << "[SettingMgr::Read] Could not open file '" << filename << "'" << endl;
    return;
  }

  Tokenizer tokens(fin);
  Tokenizer::TokenType tokenType = tokens.nextToken();

  while (tokenType != Tokenizer::eTokenEOF)
  {
    string name = tokens.getText();
    INFOLOG << "[SettingMgr::Read setting " << name << endl;
    tokenType = tokens.nextToken();

    if (tokenType == Tokenizer::eTokenFloat)
      SetFloatSetting(name, tokens.getFloat());

    else if (tokenType == Tokenizer::eTokenInteger)
      SetIntSetting(name, tokens.getNumeric());

    else if (tokenType == Tokenizer::eTokenText)
    {
      string value = tokens.getText();
      string trueStr = "true";
      if (stringNoCaseCmp(value, trueStr ))
        SetBoolSetting(name, true);
      else if (stringNoCaseCmp(value, trueStr ))
        SetBoolSetting(name, false);
      else
        SetStrSetting(name, value);
    }
  
    else
    {
      ERRORLOG << "[SettingMgr::Read] Unexpected token in Settings file '" << filename << "'" << endl;
      cout << "[SettingMgr::Read] Unexpected token in Settings file '" << filename << "'" << endl;
      exit(1);
    }
     
    tokenType = tokens.nextToken();

  }
  fin.close();
}

void SettingsMgr::Write(const string& filename)
{
  ofstream fout;
  fout.open(filename.c_str());
  if (!fout)
  {
    ERRORLOG << "[SettingMgr::Write] Could not open file '" << filename << "'" << endl;
    return;
  }

  for (map<string, string>::iterator i = mStrSettings.begin();
    i != mStrSettings.end(); i++)
  {
    fout << i->first << " \"" << i->second << "\"" << endl;
  }

  for (map<string, int>::iterator i = mIntSettings.begin();
    i != mIntSettings.end(); i++)
  {
    fout << i->first << " " << i->second << endl;
  }

  for (map<string, float>::iterator i = mRealSettings.begin();
    i != mRealSettings.end(); i++)
  {
    fout << i->first << " " << i->second << endl;
  }

  fout.close();
}

int SettingsMgr::GetIntSetting(const string& name, int alternate/*=0*/)
{
  if (mIntSettings.find(name) == mIntSettings.end())
  {
    SetIntSetting(name, alternate);
    return alternate;
  }

  return mIntSettings[name];
}

bool SettingsMgr::GetBoolSetting(const string& name, bool alternate/*=false*/)
{
  if (mIntSettings.find(name) == mIntSettings.end())
  {
    SetBoolSetting(name, alternate);
    return alternate;
  }

  return mIntSettings[name] == 1;
}

string SettingsMgr::GetStrSetting(const string& name, const string& alternate/*=""*/)
{
  if (mStrSettings.find(name) == mStrSettings.end())
  {
    SetStrSetting(name, alternate);
    return alternate;
  }

  return mStrSettings[name];
}

float SettingsMgr::GetFloatSetting(const string& name, float alternate/*=0.0f*/)
{
  if (mRealSettings.find(name) == mRealSettings.end())
  {
    SetFloatSetting(name, alternate);
    return alternate;
  }

  return mRealSettings[name];
}

void SettingsMgr::SetIntSetting(const string& name, const int value)
{
  mIntSettings[name] = value;
}

void SettingsMgr::SetFloatSetting(const string& name, const float value)
{
  mRealSettings[name] = value;
}

void SettingsMgr::SetBoolSetting(const string& name, const bool value)
{
  mIntSettings[name] = value ? 1 : 0;
}

void SettingsMgr::SetStrSetting(const string& name, const string& value)
{
  mStrSettings[name] = value;
}

} // namespace
