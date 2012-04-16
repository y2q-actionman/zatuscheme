#ifndef LISP_PTR_I_HH
#define LISP_PTR_I_HH

#ifndef LISP_PTR_HH
#error "Please include via parent file"
#endif

#include <climits>
#include <type_traits>
#include "decl.hh"

// Type mapping
template<>
struct to_type<Ptr_tag>{
  template<Ptr_tag t> struct get;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::boolean>{
  typedef bool type;
};

template<> template<>
struct to_type<Ptr_tag>::get<Ptr_tag::character>{
  typedef char type;
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

template<>
inline constexpr
Lisp_ptr::Lisp_ptr<bool>(bool b)
: tag_(to_tag<Ptr_tag, bool>()), u_(b){}

template<>
inline constexpr
Lisp_ptr::Lisp_ptr<char>(char c)
: tag_(to_tag<Ptr_tag, char>()), u_(c){}

template<>
inline constexpr
Lisp_ptr::Lisp_ptr<Ptr_tag>(Ptr_tag p)
: tag_(p), u_(nullptr){}

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

template<typename T>
inline
T Lisp_ptr::get() const {
  return (tag() == to_tag<Ptr_tag, T>())
    ? static_cast<T>(u_.ptr_) : nullptr;
}

#endif // LISP_PTR_I_HH

