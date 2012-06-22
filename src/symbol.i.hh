#ifndef SYMBOL_I_HH
#define SYMBOL_I_HH

#ifndef SYMBOL_HH
#error "Please include via parent file"
#endif

inline
void Symbol::rebind(const std::string* s){
  name_ = s;
}

#endif // SYMBOL_I_HH
