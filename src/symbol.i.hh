#ifndef SYMBOL_I_HH
#define SYMBOL_I_HH

#ifndef SYMBOL_HH
#error "Please include via parent file"
#endif

#include <utility>
#include "keyword.hh"

inline constexpr
Symbol::Symbol()
  : name_(nullptr){}

inline
Symbol::Symbol(const std::string* s)
  : name_(s){}

inline
Keyword to_keyword(const Symbol& s){
  return to_keyword(s.name().c_str());
}

inline
void Symbol::rebind(const std::string* s){
  name_ = s;
}

#endif // SYMBOL_I_HH
