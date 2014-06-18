//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// logfile.cpp
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#include "LogFile.h"

using std::cout;

namespace SEANLIB {
namespace fs = boost::filesystem;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LOG* GlobalLogObject::mLog = NULL;   // Used for global application log

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//LOG::LOG() :
//  mLevel    (eNormal),
//  mLogLevel (eNormal),
//  mNewLine  (true)
//{
//  cout << "[LOG::LOG] Created new LOG object" << endl;
//  mLog = new fs::ofstream("log");
//}

LOG::LOG(const std::string& filename) :
  mLevel    (eNormal),
  mLogLevel (eNormal),
  mNewLine  (true)
{
  Open(filename);
}

LOG::~LOG()
{
  cout << "[LOG::~LOG] Destroying LOG object" << endl;
  mLog->flush();
  mLog->close();
  delete mLog;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void LOG::Open(const std::string& filename)
{
  cout << "[LOG::Open] Sending LOG output to file " << filename << endl;
  mLog = new fs::ofstream(filename);
  *mLog << "[LOG::Open] Opening Log file" << endl;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LOG& LOG::DEBUGLEVEL()  
{ 
  SetLevel(eDebug);
  SetNewLine();
  return *this;
}
LOG& LOG::INFOLEVEL()   
{
  SetLevel(eInfo); 
  SetNewLine();
  return *this;
}
LOG& LOG::WARNINGLEVEL()
{
  SetLevel(eWarning); 
  SetNewLine();
  return *this;
}
LOG& LOG::NORMALLEVEL()
{
  SetLevel(eNormal); 
  SetNewLine();
  return *this;
}
LOG& LOG::ERRORLEVEL()
{
  SetLevel(eError); 
  SetNewLine();
  return *this;
}
LOG& LOG::CRITICALLEVEL()
{
  SetLevel(eCritical); 
  SetNewLine();
  return *this;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void LOG::SetLogLevel(eLevel newLevel)
{
  switch (newLevel) 
  {
  case eDebug :
    NORMALLEVEL() << "[LOG::SetLogLevel] Setting Log Level to Debug" << endl;
    break;
  case eInfo :
    NORMALLEVEL() << "[LOG::SetLogLevel] Setting Log Level to Info" << endl;
    break;
  case eWarning :
    NORMALLEVEL() << "[LOG::SetLogLevel] Setting Log Level to Warning" << endl;
    break;
  case eNormal :
    NORMALLEVEL() << "[LOG::SetLogLevel] Setting Log Level to Normal" << endl;
    break;
  case eError :
    NORMALLEVEL() << "[LOG::SetLogLevel] Setting Log Level to Error" << endl;
    break;
  case eCritical :
    NORMALLEVEL() << "[LOG::SetLogLevel] Setting Log Level to Critical" << endl;
    break;
  default :
    NORMALLEVEL() << "[LOG::SetLogLevel] Setting Log Level to an unknown level: " << (int)newLevel << endl;
    break;
  }
  mLogLevel = newLevel;
  mNewLine = true;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
std::string LOG::WriteHeading()
{
  std::ostringstream heading;
  if (mNewLine)
  {
    switch (mLevel) 
    {
    case eDebug :
      heading << "<     DEBUG> ";
      break;
    case eInfo :
      heading << "<    INFO  > ";
      break;
    case eWarning :
      heading << "<   WARNING> ";
      break;
    case eNormal :
      heading << "<  NORMAL  > ";
      break;
    case eError :
      heading << "< ERROR    > ";
      break;
    case eCritical :
      heading << "<CRITICAL  > ";
      break;
    default :
      heading << "<          > ";
      break;
    }
    mNewLine = false;
  }
  return (heading.str());
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
} //namespace
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
