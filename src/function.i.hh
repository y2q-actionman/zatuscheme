#ifndef FUNCTION_I_HH
#define FUNCTION_I_HH

#ifndef FUNCTION_I_HH
#error "Please include via parent file"
#endif

#include "decl.hh"

// Type mapping
template<>
struct to_type<Function::Type>{
  template<Function::Type t> struct get;
};

template<> template<>
struct to_type<Function::Type>::get<Function::Type::interpreted>{
  typedef Lisp_ptr type;
};

template<> template<>
struct to_type<Function::Type>::get<Function::Type::native>{
  typedef Function::NativeFunc type;
};


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
