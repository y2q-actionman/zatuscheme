#ifndef CONS_HH
#define CONS_HH

#include "lisp_ptr.hh"

namespace zs {

struct Cons {
  static constexpr Lisp_ptr NIL = Lisp_ptr{static_cast<Cons*>(nullptr)};

  constexpr Cons(Lisp_ptr a, Lisp_ptr d) : car(a), cdr(d){}
  Cons(const Cons&) = default;
  Cons(Cons&&) = default;

  ~Cons() = default;

  Cons& operator=(const Cons&) = default;
  Cons& operator=(Cons&&) = default;

  Lisp_ptr car;
  Lisp_ptr cdr;
};

} // namespace zs

#endif // CONS_HH
