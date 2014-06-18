//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//  LogFile.h
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ifndef _SEANLIB_LOGFILE
#define _SEANLIB_LOGFILE

#include <sstream>
#include <string>

#include <boost/filesystem.hpp>   // includes all needed Boost.Filesystem declarations
#include <boost/filesystem/fstream.hpp>
#include <boost/thread.hpp>

#include <iostream>               // for std::cout

namespace SEANLIB {

//using namespace std;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define LOGOBJECT   SEANLIB::GlobalLogObject::GetLog()
#define CLOSELOG    SEANLIB::GlobalLogObject::CloseLog()
#define DEBUGLOG    LOGOBJECT.DEBUGLEVEL()
#define INFOLOG     LOGOBJECT.INFOLEVEL()
#define NORMALLOG   LOGOBJECT.NORMALLEVEL()
#define WARNINGLOG  LOGOBJECT.WARNINGLEVEL()
#define ERRORLOG    LOGOBJECT.ERRORLEVEL()
#define CRITICALLOG LOGOBJECT.CRITICALLEVEL()
#define LOGCONTEXT SEANLIB::LogContext _logContext

#define endl "\n"
#define MAXFILENAME 255

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
enum eLevel { eDebug, eInfo, eWarning, eNormal, eError, eCritical };

class LOG
{
public:
  //LOG();
  LOG(const std::string& filename);
  ~LOG();

  LOG& DEBUGLEVEL();
  LOG& INFOLEVEL();
  LOG& WARNINGLEVEL();
  LOG& NORMALLEVEL();
  LOG& ERRORLEVEL();
  LOG& CRITICALLEVEL();

  void SetLogLevel(eLevel newLevel);
  void SetNewLine() { mNewLine = true; }

  template <typename T> LOG& operator << (const T& obj);

protected:
  void Open(const std::string& filename);

private:
  boost::mutex io_mutex;
  eLevel    mLevel;
  eLevel    mLogLevel;
  bool      mNewLine;

  boost::filesystem::ofstream* mLog;


  void SetLevel(eLevel newLevel) { mLevel = newLevel; }
  std::string WriteHeading();
};

class GlobalLogObject
{
public:
  GlobalLogObject();
  ~GlobalLogObject() 
  { 
    CloseLog();
  }

  static LOG& OpenLog(const std::string& filename) 
  { 
    mLog = new LOG(filename);
    return *mLog; 
  }

  static void CloseLog()
  {
    if (mLog != NULL)
    {
      delete mLog;
    }
  }

  static LOG& GetLog() 
  { 
    if (mLog == NULL)
      mLog = new LOG("default.log");
    return *mLog; 
  }
private:
  static LOG* mLog;
};

class LogContext
{
public:
  LogContext(const std::string& method) :
    mMethod(method)
  {
    GlobalLogObject::GetLog().NORMALLEVEL() << "[" << mMethod << "] ->" << endl;
  }
  ~LogContext()
  {
    GlobalLogObject::GetLog().NORMALLEVEL() << "[" << mMethod << "] <-" << endl;
  }
private:
  std::string mMethod;
};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

template <typename T> 
inline LOG& LOG::operator << (const T& obj)
{
  if (mLevel >= mLogLevel)
  {
    boost::mutex::scoped_lock lock(io_mutex);
    //std::ostringstream strstream;
    //strstream << WriteHeading() << obj;
    //mLog->write(strstream.str(), strstream.width());
    *mLog << WriteHeading();
    *mLog << obj;
    mLog->flush();
  }
  else
  { 
    // dump string
    std::ostringstream dump;
    dump << obj;
  }
  return *this;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
} // namespace
#endif  // _SEANLIB_LOGFILE

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
