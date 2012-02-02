#include "symbol.hh"

#include "keyword.hh"

using namespace std;

Keyword Symbol::keyword() const{
  return to_keyword(name_.c_str());
}

