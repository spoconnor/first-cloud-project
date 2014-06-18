//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Chunk.h
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ifndef _SEANLIB_CHUNK
#define _SEANLIB_CHUNK

#include <string>
#include <iostream>

namespace SEANLIB {
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class Chunk {
public:
  enum TokenType {eTokenOther, eTokenInteger, eTokenFloat, eTokenQuotedString, eTokenText, eTokenEOL, eTokenEOF, eTokenBeginGroup, eTokenEndGroup, eTokenSeparator};

  Chunk()
  { }
  
private:
  byte GetBlock(int x, int y, int z);

private:
  std::list<byte> blocks; 
};
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

inline std::ostream &operator << (std::ostream &out, Tokenizer::TokenType type) {
  switch (type) {
  case Tokenizer::eTokenOther:
    return out << "character";
  case Tokenizer::eTokenQuotedString:
    return out << "quoted string";
  default:
    return out << "Unknown token type";
  }
  return out;
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
} // Sean

#endif // _SEANLIB_TOKEN
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
