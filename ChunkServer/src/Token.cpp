//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Token.cpp
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#include "Token.h"

namespace SEANLIB {
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tokenizer::TokenType Tokenizer::nextToken() 
{
  int next = input.get();
  bool cont;
  tt = eTokenOther;
  do 
  {
    cont = false;
    //NORMALLOG << "Next char: " << next << " (" << (char) next << ")" << endl;
    if (next == EOF) 
    {
      tt = eTokenEOF;
    } 
    // Comment
    else if (next == comment) 
    {
      do 
      {
        next = input.get();
      } while (next != EOF && next != '\n');

      if (next == '\n') 
      {
        next = input.get();
        ++line;
      }
      cont = true;
    }
    // Begin Grouping
    else if (next=='[' || next=='{' || next=='(')
    {
      groupChar = next;
      next=input.get();
      tt = eTokenBeginGroup;
    }
    // End Grouping
    else if (next==']' || next=='}' || next==')')
    {
      groupChar = next;
      next=input.get();
      tt = eTokenEndGroup;
    }
 
    // Separator
    else if (next==separator)
    {
      next=input.get();
      tt = eTokenSeparator;
    }
    // Quoted String
    else if (next == quote) 
    {
      std::ostringstream buf;
      next = input.get();
      while (next != EOF && next != '\n' && next != quote) 
      {
        // cerr << "Next string char: " << next << " (" << (char) next << ")" << endl;
        buf << (char) next;
        next = input.get();
      }
      if (next != quote)
	  {
        ERRORLOG << "Unterminated string" << endl;
	  }
      text = buf.str();
      tt = eTokenQuotedString;
    } 
    // White Space
    else if (isspace(next) && ignoreWS) 
    {
      while (next != EOF && isspace (next)) 
      {
        if (next == '\n')
          ++line;
        next = input.get();
      }
      cont = true;
    }
    // Integer or Float
    else if (isdigit(next) || next=='-' || next=='+') 
    {
      std::ostringstream buf;
      while (isdigit(next) || next=='+' || next=='-' || next=='.' || next=='e')
      {
        if (next=='.' || next=='e' || next=='-')
        {
          tt = eTokenFloat;
        }  
        buf << (char) next;
        next = input.get();
      }
      input.putback (next);
  
      if (tt == eTokenFloat)
      {
        realNum = atof (buf.str().c_str());
      }
      else
      {
        tt = eTokenInteger;
        numeric = atoi (buf.str().c_str());
      }
     
    } 
    // String
    else if (isalpha (next)) 
    {
      std::ostringstream buf;
      while (isalnum (next) || next == '_') 
      {
        buf << (char) next;
        next = input.get();
      }
      input.putback (next);
      text = buf.str();
      tt = eTokenText;
    } 
    // End of Line
    else if (next=='\n')
    {
      next=input.get();
      tt = eTokenEOL;
      if (ignoreEOL)
        cont=true;
    }
    // Other Unknown token
    else 
    {
      ch = next;
      tt = eTokenOther;
    }
  } while (cont);
  
  return tt;
}
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tokenizer::TokenType Tokenizer::getToken() 
{
  return tt;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

std::string Tokenizer::getNextStringToken()
{
  std::string value = "";
  TokenType tokenType = nextToken();

  if (tokenType != Tokenizer::eTokenEOF)
  {
    value = getText();
  }
  return value;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

std::string Tokenizer::readStringToEOL()
{
  std::string value = "";

  int next = input.get();
  while (next != EOF && next != '\n')
  {
    value += char(next);
    next = input.get();
  } 
  
  return value;  
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void Tokenizer::skipToEOL()
{
  int next;
  do 
  {
    next = input.get();
  } while (next != EOF && next != '\n');
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
std::string Tokenizer::readText() {
  TokenType tokenType = nextToken();
  if (tokenType != eTokenText && tokenType != eTokenQuotedString) {
    ERRORLOG << "Expected a Text, got a " << tt << endl;
  }
  return text;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
char Tokenizer::readChar() {
  if (nextToken() != eTokenOther) {
	ERRORLOG << "Expected an character, got a " << tt << endl;
  }
  return ch;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
long Tokenizer::readNumeric() {
  if (nextToken() != eTokenInteger) {
    ERRORLOG << "Expected an integer, got a " << tt << endl;
  }
  return numeric;
}
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
float Tokenizer::readFloat() {
  if (nextToken() != eTokenFloat) {
    ERRORLOG << "Expected a float, got a " << tt << endl;
  }
  return realNum;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void Tokenizer::expectEOF() {
  if (nextToken() != eTokenEOF) {
    ERRORLOG << "Expected End-of-file, got a " << tt << endl;
  }
}
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void Tokenizer::expectChar (char c) {
  if (nextToken() != eTokenOther || ch != c) {
    ERRORLOG << "Expected '" << c << "', got a ";
    if (tt == eTokenOther)
      ERRORLOG << "'" << ch << "'" << endl;
    else
      ERRORLOG << tt << endl;
  }
}
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
char Tokenizer::expectCharIn (const std::string &str) {
  if (nextToken() != eTokenOther || str.find (ch) == std::string::npos) {
    ERRORLOG << "Expected one of \"" << str << "\", got a ";
    if (tt == eTokenOther)
      ERRORLOG << "'" << ch << "'" << endl;
    else
      ERRORLOG << tt << endl;
  }
  return ch;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void Tokenizer::expectQuotedString (const std::string &str) {
  if (nextToken() != eTokenQuotedString || text != str) {
    ERRORLOG << "Expected " << quote << str << quote << ", got ";
    if (tt == eTokenQuotedString)
      ERRORLOG << quote << text << quote << endl;
    else
      ERRORLOG << " a " << tt << endl;
  }
}
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void Tokenizer::expectText (const std::string &str) {
  if (nextToken() != eTokenText || text != str) {
    ERRORLOG << "Expected " << str << ", got ";
    if (tt == eTokenText)
      ERRORLOG << text << endl;
    else
      ERRORLOG << " a " << tt << endl;
  }
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

} // namespace
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
