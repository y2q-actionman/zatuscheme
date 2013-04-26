#ifndef CONS_HH
#define CONS_HH

#include "lisp_ptr.hh"

struct Cons {
  friend class GrowList;
  friend class ConsIter;

public:
  static constexpr Lisp_ptr NIL = Lisp_ptr{static_cast<Cons*>(nullptr)};

  constexpr Cons(Lisp_ptr = Lisp_ptr(), Lisp_ptr = Lisp_ptr());
  Cons(const Cons&) = default;
  Cons(Cons&&) = default;

  ~Cons() = default;

  Cons& operator=(const Cons&) = default;
  Cons& operator=(Cons&&) = default;

  friend Lisp_ptr car(Cons*);
  friend Lisp_ptr cdr(Cons*);
  friend void rplaca(Cons*, Lisp_ptr);
  friend void rplacd(Cons*, Lisp_ptr);

private:
  Lisp_ptr car_;
  Lisp_ptr cdr_;
};

#include "cons.i.hh"

#endif // CONS_HH
