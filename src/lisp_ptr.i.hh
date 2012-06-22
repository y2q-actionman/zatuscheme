#ifndef LISP_PTR_I_HH
#define LISP_PTR_I_HH

#ifndef LISP_PTR_HH
#error "Please include via parent file"
#endif

#include "decl.hh"

// Type mapping
template<Ptr_tag t, typename T>
T to_type() = delete;

template<>
bool to_type<Ptr_tag::boolean>() = delete;

template<>
char to_type<Ptr_tag::character>() = delete;

template<>
Cons* to_type<Ptr_tag::cons>() = delete;

template<>
Symbol* to_type<Ptr_tag::symbol>() = delete;

template<>
Procedure::Function* to_type<Ptr_tag::function>() = delete;

template<>
Number* to_type<Ptr_tag::number>() = delete;

template<>
String* to_type<Ptr_tag::string>() = delete;

template<>
Vector* to_type<Ptr_tag::vector>() = delete;

template<>
Port* to_type<Ptr_tag::port>() = delete;

template<>
Env* to_type<Ptr_tag::env>() = delete;

template<>
VM_op to_type<Ptr_tag::vm_op>() = delete;


template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, bool>(){
  return Ptr_tag::boolean;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, char>(){
  return Ptr_tag::character;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Cons*>(){
  return Ptr_tag::cons;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Symbol*>(){
  return Ptr_tag::symbol;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Procedure::Function*>(){
  return Ptr_tag::function;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Number*>(){
  return Ptr_tag::number;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, String*>(){
    return Ptr_tag::string;
  }

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Vector*>(){
  return Ptr_tag::vector;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Port*>(){
  return Ptr_tag::port;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Env*>(){
  return Ptr_tag::env;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, VM_op>(){
  return Ptr_tag::vm_op;
}


// ptr class definitions

template<>
inline constexpr
Lisp_ptr::Lisp_ptr<Ptr_tag>(Ptr_tag p)
: tag_(p), u_(){}

template<typename T>
inline constexpr
Lisp_ptr::Lisp_ptr(T p)
  : tag_(to_tag<Ptr_tag, T>()), u_(p){}


template<>
inline
bool Lisp_ptr::get<bool>() const {
  return (tag() == to_tag<Ptr_tag, bool>())
    ? u_.b_
    : true; // anything is #t, except #f and null
}

template<>
inline
char Lisp_ptr::get<char>() const {
  return (tag() == to_tag<Ptr_tag, char>())
    ? u_.c_ : '\0';
}

template<>
inline
VM_op Lisp_ptr::get<VM_op>() const {
  return (tag() == to_tag<Ptr_tag, VM_op>()
     ? u_.f_ : nullptr);
}

template<>
inline
void* Lisp_ptr::get<void*>() const{
  return u_.ptr_;
}

template<typename T>
inline
T Lisp_ptr::get() const {
  return (tag() == to_tag<Ptr_tag, T>())
    ? static_cast<T>(u_.ptr_) : nullptr;
}

#endif // LISP_PTR_I_HH
