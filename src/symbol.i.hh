#ifndef SYMBOL_I_HH
#define SYMBOL_I_HH

#ifndef SYMBOL_HH
#error "Please include via parent file"
#endif

#include <utility>

inline
Symbol::Symbol(const std::string& s)
  : name_(s), k_(to_keyword(s.c_str())){}

inline
Symbol::Symbol(std::string&& s)
  : name_(std::forward<std::string>(s)),
    k_(to_keyword(s.c_str())){}

#endif // SYMBOL_I_HH
