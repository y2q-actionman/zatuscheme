#ifndef LISP_PTR_HH
#define LISP_PTR_HH

#include <cstdint>
#include <type_traits>
#include "decl.hh"

class Lisp_ptr {
public:
  constexpr Lisp_ptr();

  template<typename T>
  explicit constexpr
  Lisp_ptr(T, typename std::enable_if<std::is_integral<T>::value>::type* = nullptr);

  template<typename T>
  constexpr
  Lisp_ptr(T, typename std::enable_if<!std::is_integral<T>::value>::type* = nullptr);

  Lisp_ptr(const Lisp_ptr&) = default;
  Lisp_ptr(Lisp_ptr&&) = default;

  ~Lisp_ptr() = default;

  Lisp_ptr& operator=(const Lisp_ptr&) = default;
  Lisp_ptr& operator=(Lisp_ptr&&) = default;

  constexpr
  Ptr_tag tag() const
  { return tag_; }

  template<typename T>
  T get() const;

  explicit constexpr operator bool() const
  { return (tag_ != Ptr_tag::undefined); }

  // storage
  union lisp_ptr_u{
    constexpr lisp_ptr_u(int i) : i_(i){}
    constexpr lisp_ptr_u(void* p) : ptr_(p){}
    constexpr lisp_ptr_u(const void* p) : cptr_(p){}
    constexpr lisp_ptr_u(void(*f)()) : f_(f){}

    intptr_t i_;
    void* ptr_;
    const void* cptr_;
    void (*f_)(void);
  };

  Ptr_tag tag_;
  lisp_ptr_u u_;
};

#include "lisp_ptr.i.hh"

#endif // LISP_PTR_HH
