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
Lisp_ptr to_type<Function::Type::interpreted_macro>() = delete;

template<>
Function::NativeFunc to_type<Function::Type::native>() = delete;

template<>
Function::NativeFunc to_type<Function::Type::native_macro>() = delete;

// type -> tag is ambiguous..


template<> inline
Lisp_ptr Function::get() const{
  switch(type_){
  case Function::Type::interpreted:
  case Function::Type::interpreted_macro:
    return code_;
  case Function::Type::native:
  case Function::Type::native_macro:
    return {};
  default:
    UNEXP_DEFAULT();
  }
}

template<> inline
Function::NativeFunc Function::get() const{
  switch(type_){
  case Function::Type::interpreted:
  case Function::Type::interpreted_macro:
    return nullptr;
  case Function::Type::native:
  case Function::Type::native_macro:
    return n_func_;
  default:
    UNEXP_DEFAULT();
  }
}

#endif // FUNCTION_I_HH
