#ifndef FUNCTION_I_HH
#define FUNCTION_I_HH

#ifndef FUNCTION_HH
#error "Please include via parent file"
#endif

#include "decl.hh"
#include "util.hh"

// Type mapping
template<Function::Type t, typename T>
T to_type() = delete;

template<>
Lisp_ptr to_type<Function::Type::interpreted>() = delete;

template<>
Function::NativeFunc to_type<Function::Type::native>() = delete;

template<>
inline constexpr
Function::Type to_tag<Function::Type, Lisp_ptr>(){
  return Function::Type::interpreted;
}

template<>
inline constexpr
Function::Type to_tag<Function::Type, Function::NativeFunc>(){
  return Function::Type::native;
}


template<> inline
Lisp_ptr Function::get() const{
  switch(type_){
  case Function::Type::interpreted:
    return code_;
  case Function::Type::native:
    return {};
  default:
    UNEXP_DEFAULT();
  }
}

template<> inline
Function::NativeFunc Function::get() const{
  switch(type_){
  case Function::Type::interpreted:
    return nullptr;
  case Function::Type::native:
    return n_func_;
  default:
    UNEXP_DEFAULT();
  }
}

#endif // FUNCTION_I_HH
