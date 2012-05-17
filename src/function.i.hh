#ifndef FUNCTION_I_HH
#define FUNCTION_I_HH

#ifndef FUNCTION_HH
#error "Please include via parent file"
#endif

#include "decl.hh"

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
  return (type_ == to_tag<Function::Type, Lisp_ptr>()) ? code_ : Lisp_ptr{};
}

template<> inline
Function::NativeFunc Function::get() const{
  return (type_ == to_tag<Function::Type, NativeFunc>()) ? n_func_ : nullptr;
}

#endif // FUNCTION_I_HH
