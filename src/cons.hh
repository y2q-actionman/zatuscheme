#ifndef CONS_HH
#define CONS_HH

#include "lisp_ptr.hh"

class Cons {
  friend class GrowList;

public:
  static constexpr Lisp_ptr NIL = Lisp_ptr{static_cast<Cons*>(nullptr)};

  constexpr Cons(Lisp_ptr = Lisp_ptr(), Lisp_ptr = Lisp_ptr());
  Cons(const Cons&) = default;
  Cons(Cons&&) = default;

  ~Cons() = default;

  Cons& operator=(const Cons&) = default;
  Cons& operator=(Cons&&) = default;

  inline Lisp_ptr car() const;
  inline Lisp_ptr cdr() const;
  inline void rplaca(Lisp_ptr);
  inline void rplacd(Lisp_ptr);

private:
  Lisp_ptr car_;
  Lisp_ptr cdr_;
};

#include "cons.i.hh"

#endif // CONS_HH
