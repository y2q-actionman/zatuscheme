#ifndef LISP_PTR_I_HH
#define LISP_PTR_I_HH

#ifndef LISP_PTR_HH
#error "Please include via parent file"
#endif

#include <climits>
#include "decl.hh"

// Type mapping
template<>
struct to_type<Ptr_tag>{
  template<Ptr_tag t> struct get;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::cons>{
  typedef Cons* type;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::symbol>{
  typedef Symbol* type;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::long_ptr>{
  typedef Long_ptr* type;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::function>{
  typedef Function* type;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::number>{
  typedef Number* type;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::string>{
  typedef String* type;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::vector>{
  typedef Vector* type;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::port>{
  typedef Port* type;
};


template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, bool>(){
  return Ptr_tag::immediate;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, char>(){
  return Ptr_tag::immediate;
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
Ptr_tag to_tag<Ptr_tag, Long_ptr*>(){
  return Ptr_tag::long_ptr;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Function*>(){
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


// ptr class definitions

namespace lisp_ptr_i {
  static constexpr uintptr_t tag_bit_mask = 0x3u;
  static constexpr size_t required_alignment = 4;
  static constexpr uintptr_t embed_boolean_bit = 0x4u;
} // namespace lisp_ptr_i

constexpr inline
bool Lisp_ptr::includes(Ptr_tag t){
  return (t == Ptr_tag::immediate)
    || (t == Ptr_tag::cons)
    || (t == Ptr_tag::symbol)
    || (t == Ptr_tag::long_ptr);
}

template<>
inline constexpr
Lisp_ptr::Lisp_ptr<bool>(bool b)
  : base_((0xffu << CHAR_BIT)
          | (b ? lisp_ptr_i::embed_boolean_bit : 0)){}

template<>
inline constexpr
Lisp_ptr::Lisp_ptr<char>(char c)
  : base_((static_cast<unsigned>(c) << CHAR_BIT)
          | lisp_ptr_i::embed_boolean_bit){}

template<>
inline constexpr
Lisp_ptr::Lisp_ptr<Ptr_tag>(Ptr_tag p)
: base_((static_cast<unsigned>(p) <= lisp_ptr_i::tag_bit_mask)
        ? (reinterpret_cast<uintptr_t>(nullptr) | static_cast<uintptr_t>(p))
        : 0){}

template<typename T>
inline constexpr
Lisp_ptr::Lisp_ptr(T p)
  : base_(reinterpret_cast<uintptr_t>(p)
          | static_cast<uintptr_t>(to_tag<Ptr_tag, T>())){
  static_assert(static_cast<unsigned>(to_tag<Ptr_tag, T>())
                <= lisp_ptr_i::tag_bit_mask,
                "Lisp_ptr cannot be used with specified type");
  static_assert(alignof(T) <= lisp_ptr_i::required_alignment,
                "Lisp_ptr cannot be used with misaligned type");
}


inline
Ptr_tag Lisp_ptr::tag() const {
  switch(base_ & lisp_ptr_i::tag_bit_mask){
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
    ? ((base_ & lisp_ptr_i::embed_boolean_bit) != 0)
    : true; // anything is #t, except #f and null
}

template<>
inline
char Lisp_ptr::get<char>() const {
  return (tag() == Ptr_tag::immediate)
    ? ((base_ >> CHAR_BIT) & ((1u << CHAR_BIT) - 1u))
    : '\0';
}

template<typename T>
inline
T Lisp_ptr::get() const {
  return (tag() == to_tag<Ptr_tag, T>())
    ? static_cast<T>(reinterpret_cast<void*>(base_ & ~lisp_ptr_i::tag_bit_mask))
    : nullptr;
}

inline
bool Lisp_ptr::is_bool() const{
  return (get<char>() == static_cast<char>(0xff));
}


// Long_ptr definitions
template<typename T>
inline constexpr
Long_ptr::Long_ptr(T p)
  : tag_(to_tag<Ptr_tag, T>()), ptr_(p){}
  
template<typename T>
inline
T Long_ptr::get() const {
  return (tag() == to_tag<Ptr_tag, T>())
    ? static_cast<T>(ptr_)
    : nullptr;
}

#endif // LISP_PTR_I_HH

