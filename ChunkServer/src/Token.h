//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Token.h
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ifndef _SEANLIB_TOKEN
#define _SEANLIB_TOKEN

#include <string>
#include <iostream>

#include "LogFile.h"

namespace SEANLIB {
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class Tokenizer {
public:
  enum TokenType {eTokenOther, eTokenInteger, eTokenFloat, eTokenQuotedString, eTokenText, eTokenEOL, eTokenEOF, eTokenBeginGroup, eTokenEndGroup, eTokenSeparator};
  
  Tokenizer (std::istream &in):
    input (in),
    quote ('"'),
    comment ('#'),
    ignoreWS (true), 
    ignoreEOL (false),
    tt (eTokenEOF),
    ch (0),
    numeric (0), 
    realNum (0.0),
    line (1),
    separator (',')
  { }
  
  operator bool () const 
  {
    return !(tt == Tokenizer::eTokenEOF || input.peek() == EOF);
  }
  
  TokenType nextToken();
  TokenType getToken();
  void skipToEOL();
  
  std::string getText() const;
  long getNumeric() const;
  float getFloat() const;
  char getCharacter() const;
  char getGroupChar() const { return groupChar; }
  
  std::string getNextStringToken();
  std::string readStringToEOL();
  
  void setQuoteChar (int q = '\"')  { quote = q; }
  void setCommentChar (int c = '#') { comment = c; }
  void setIgnoreWS (bool ws = true) { ignoreWS = ws; }
  
  char readChar();
  std::string readText();
  long readNumeric();
  float readFloat();
  
  void expectEOF();
  void expectChar (char c);
  char expectCharIn (const std::string &str);
  void expectQuotedString (const std::string &str);
  void expectText (const std::string &str);
    
  long getLine() const { return line; }
    
private:
  std::istream &input;
  char quote, comment;
  bool ignoreWS;
  bool ignoreEOL;
  std::string text;
  TokenType tt;
  char ch;
  long numeric;
  float realNum;
  long line;
  char separator;
  char groupChar;
};
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

inline std::ostream &operator << (std::ostream &out, Tokenizer::TokenType type) {
  switch (type) {
  case Tokenizer::eTokenOther:
    return out << "character";
  case Tokenizer::eTokenQuotedString:
    return out << "quoted string";
  case Tokenizer::eTokenInteger:
    return out << "integer";
  case Tokenizer::eTokenFloat:
    return out << "float";
  case Tokenizer::eTokenText:
    return out << "text";
  case Tokenizer::eTokenEOF:
    return out << "End-of-file";
  case Tokenizer::eTokenEOL :
    return out << "End-of-line";
  case Tokenizer::eTokenBeginGroup :
    return out << "Begin Group";
  case Tokenizer::eTokenEndGroup :
    return out << "End Group";
  case Tokenizer::eTokenSeparator :
    return out << "Data Separator";
  default:
    return out << "Unknown token type";
  }
  return out;
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
inline std::string Tokenizer::getText() const 
{
  std::ostringstream buf;
  switch (tt) {
  case eTokenOther:
    buf << (char) ch;
    return buf.str();
  case eTokenQuotedString:
  case eTokenText:
    return text;
  case eTokenInteger:
    buf << numeric;
    return buf.str();
  case eTokenFloat:
    buf << realNum;
    return buf.str();
  case eTokenEOF:
    return "";
  default:
	ERRORLOG << "Unknown token type" << endl;
  }
    
  // To keep the compiler happy
  return "";
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

inline long Tokenizer::getNumeric() const {
  switch (tt) {
  case eTokenOther:
  case eTokenQuotedString:
  case eTokenText:
  case eTokenEOF:
    return 0;
  case eTokenFloat:
    return long(realNum);
  case eTokenInteger:
    return numeric;
  default:
    ERRORLOG << "Unknown token type" << endl;
  }

  // To keep the compiler happy
  return 0;
}
    
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

inline float Tokenizer::getFloat() const {
  switch (tt) {
  case eTokenOther:
  case eTokenQuotedString:
  case eTokenText:
  case eTokenEOF:
  case eTokenInteger:
    return float(numeric);
  case eTokenFloat:
    return realNum;
  default:
    ERRORLOG << "Unknown token type" << endl;
  }

  // To keep the compiler happy
  return 0;
}
    
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

inline char Tokenizer::getCharacter() const {
  switch (tt) {
  case eTokenOther:
    return ch;
  case eTokenQuotedString:
  case eTokenText:
  case eTokenInteger:
  case eTokenFloat:
  case eTokenEOF:
    return 0;
  default:
    ERRORLOG << "Unknown token type" << endl;
  }
      
  // To keep the compiler happy
  return 0;
}
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
} // Sean

#endif // _SEANLIB_TOKEN
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
