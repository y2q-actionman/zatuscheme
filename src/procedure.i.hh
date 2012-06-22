#ifndef PROCEDURE_I_HH
#define PROCEDURE_I_HH

#ifndef PROCEDURE_HH
#error "Please include via parent file"
#endif

#include "decl.hh"
#include "util.hh"

// Type mapping
template<Procedure::Type t, typename T>
T to_type() = delete;

template<>
Lisp_ptr to_type<Procedure::Type::interpreted>() = delete;

template<>
Procedure::NativeFunc to_type<Procedure::Type::native>() = delete;

template<>
inline constexpr
Procedure::Type to_tag<Procedure::Type, Lisp_ptr>(){
  return Procedure::Type::interpreted;
}

template<>
inline constexpr
Procedure::Type to_tag<Procedure::Type, Procedure::NativeFunc>(){
  return Procedure::Type::native;
}

#endif // PROCEDURE_I_HH
