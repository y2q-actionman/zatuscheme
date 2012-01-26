#ifndef LISP_PTR_I_HH
#define LISP_PTR_I_HH

#ifndef LISP_PTR_HH
#error "Please include via parent file"
#endif

#include <climits>
#include "keyword.hh"

class Cons;
class Symbol;
class Long_ptr;
class Function;
class Number;
class String;
class Vector;
class Port;

namespace lisp_ptr_i {
  template<typename T>
  Ptr_tag Type_to_Ptr_tag();

  template<>
  inline constexpr
  Ptr_tag Type_to_Ptr_tag<Cons*>(){
    return Ptr_tag::cons;
  }

  template<>
  inline constexpr
  Ptr_tag Type_to_Ptr_tag<Symbol*>(){
    return Ptr_tag::symbol;
  }

  template<>
  inline constexpr
  Ptr_tag Type_to_Ptr_tag<Long_ptr*>(){
    return Ptr_tag::long_ptr;
  }

  template<>
  inline constexpr
  Ptr_tag Type_to_Ptr_tag<Function*>(){
    return Ptr_tag::function;
  }

  template<>
  inline constexpr
  Ptr_tag Type_to_Ptr_tag<Number*>(){
    return Ptr_tag::number;
  }

  template<>
  inline constexpr
  Ptr_tag Type_to_Ptr_tag<String*>(){
    return Ptr_tag::string;
  }

  template<>
  inline constexpr
  Ptr_tag Type_to_Ptr_tag<Vector*>(){
    return Ptr_tag::vector;
  }

  template<>
  inline constexpr
  Ptr_tag Type_to_Ptr_tag<Port*>(){
    return Ptr_tag::port;
  }
} // namespace lisp_ptr_i


template<>
inline
Lisp_ptr::Lisp_ptr<bool>(bool b)
  : base_((0xffu << CHAR_BIT)
          | (b ? embed_boolean_bit : 0)){}

template<>
inline
Lisp_ptr::Lisp_ptr<char>(char c)
  : base_((static_cast<unsigned>(c) << CHAR_BIT)
          | embed_boolean_bit){}

template<typename T>
inline
Lisp_ptr::Lisp_ptr(T p)
  : ptr_(p){
  static_assert(static_cast<unsigned>(lisp_ptr_i::Type_to_Ptr_tag<T>())
                <= tag_bit_mask,
                "Lisp_ptr cannot be used with specified type");
  base_ |= static_cast<uintptr_t>(lisp_ptr_i::Type_to_Ptr_tag<T>());
}


inline
Ptr_tag Lisp_ptr::tag() const {
  switch(base_ & tag_bit_mask){
  case 0x0:
    return Ptr_tag::immediate;
  case 0x1:
    return Ptr_tag::cons;
  case 0x2:
    return Ptr_tag::symbol;
  case 0x3:
    return Ptr_tag::long_ptr;
  default:
    return Ptr_tag::unknown;
  }
}

template<>
inline
bool Lisp_ptr::get<bool>() const {
    return (tag() == Ptr_tag::immediate)
    ? ((base_ & embed_boolean_bit) != 0)
    : true; // everything is #t, except #f and null
}

template<>
inline
char Lisp_ptr::get<char>() const {
  return (tag() == Ptr_tag::immediate)
    ? ((base_ >> CHAR_BIT) & (CHAR_BIT - 1))
    : '\0';
}

template<typename T>
inline
T Lisp_ptr::get() const {
  return (tag() == lisp_ptr_i::Type_to_Ptr_tag<T>())
    ? static_cast<T>(reinterpret_cast<void*>(base_ & ~tag_bit_mask))
    : nullptr;
}


template<typename T>
inline
Long_ptr::Long_ptr(T p)
  : tag_(lisp_ptr_i::Type_to_Ptr_tag<T>()), ptr_(p){}
  
template<typename T>
inline
T Long_ptr::get() const {
  return (tag() == lisp_ptr_i::Type_to_Ptr_tag<T>())
    ? static_cast<T>(ptr_)
    : nullptr;
}

#endif // LISP_PTR_I_HH

