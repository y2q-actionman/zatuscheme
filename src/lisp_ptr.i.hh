#ifndef LISP_PTR_I_HH
#define LISP_PTR_I_HH

#ifndef LISP_PTR_HH
#error "Please include via parent file"
#endif

#include <type_traits>
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
Procedure::IProcedure* to_type<Ptr_tag::i_procedure>() = delete;

template<>
const Procedure::NProcedure* to_type<Ptr_tag::n_procedure>() = delete;

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
Delay* to_type<Ptr_tag::delay>() = delete;

template<>
Procedure::Continuation* to_type<Ptr_tag::continuation>() = delete;

template<>
VMop to_type<Ptr_tag::vm_op>() = delete;


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
Ptr_tag to_tag<Ptr_tag, Procedure::IProcedure*>(){
  return Ptr_tag::i_procedure;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, const Procedure::NProcedure*>(){
  return Ptr_tag::n_procedure;
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
Ptr_tag to_tag<Ptr_tag, Delay*>(){
  return Ptr_tag::delay;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Procedure::Continuation*>(){
  return Ptr_tag::continuation;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, VMop>(){
  return Ptr_tag::vm_op;
}


// ptr class definitions

inline constexpr
Lisp_ptr::Lisp_ptr(bool b)
  : tag_(to_tag<Ptr_tag, bool>()), u_(b){}

inline constexpr
Lisp_ptr::Lisp_ptr(char c)
  : tag_(to_tag<Ptr_tag, char>()), u_(c){}

template<typename T>
inline constexpr
Lisp_ptr::Lisp_ptr(T p)
  : tag_(to_tag<Ptr_tag, T>()), u_(p){
  static_assert(!std::is_fundamental<T>::value,
                "Lisp_ptr cannot accept the specified type.");
}


template<>
inline constexpr
bool Lisp_ptr::get<bool>() const {
  return (tag() == to_tag<Ptr_tag, bool>())
    ? u_.b_
    : operator bool(); // anything is #t, except #f and null
}

template<>
inline constexpr
char Lisp_ptr::get<char>() const {
  return (tag() == to_tag<Ptr_tag, char>())
    ? u_.c_ : '\0';
}

template<>
inline constexpr
VMop Lisp_ptr::get<VMop>() const {
  return (tag() == to_tag<Ptr_tag, VMop>()
     ? u_.f_ : nullptr);
}

template<>
inline constexpr
void* Lisp_ptr::get<void*>() const{
  return u_.ptr_;
}

namespace lisp_ptr_detail {

template<typename T>
inline constexpr
T lisp_ptr_cast(void* p, const void*,
                typename std::enable_if<
                  !std::is_const<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr){
  return static_cast<T>(p);
}

template<typename T>
inline constexpr
T lisp_ptr_cast(void*, const void* cp,
                typename std::enable_if<
                  std::is_const<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr){
  return static_cast<T>(cp);
}

}

template<typename T>
inline constexpr
T Lisp_ptr::get() const {
  return (tag() == to_tag<Ptr_tag, T>())
    ? lisp_ptr_detail::lisp_ptr_cast<T>(u_.ptr_, u_.cptr_)
    : nullptr;
}

#endif // LISP_PTR_I_HH
