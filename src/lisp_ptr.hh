#ifndef LISP_PTR_HH
#define LISP_PTR_HH

#include <cstdint>
#include "decl.hh"

class Lisp_ptr {
public:
  constexpr Lisp_ptr() : tag_(Ptr_tag::undefined), u_(){}
  explicit constexpr Lisp_ptr(bool); // fundamental types are 'explicit'
  explicit constexpr Lisp_ptr(char);
  explicit constexpr Lisp_ptr(int);
  template<typename T> constexpr Lisp_ptr(T); // non-fundamental type

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

private:
  union lisp_ptr_u{
    constexpr lisp_ptr_u() : i_(0){}
    constexpr lisp_ptr_u(void* p) : ptr_(p){}
    constexpr lisp_ptr_u(const void* p) : cptr_(p){}
    constexpr lisp_ptr_u(bool b) : i_(b){}
    constexpr lisp_ptr_u(char c) : i_(c){}
    constexpr lisp_ptr_u(int i) : i_(i){}
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
