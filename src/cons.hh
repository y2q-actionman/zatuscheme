#ifndef CONS_HH
#define CONS_HH

#include "lisp_ptr.hh"

class Cons {
public:
  static const Lisp_ptr NIL;

  Cons() = delete;
  explicit Cons(Lisp_ptr); // one element list
  Cons(Lisp_ptr, Lisp_ptr);
  Cons(const Cons&) = default;
  Cons(Cons&&) = default;

  ~Cons() = default;

  Cons& operator=(const Cons&) = default;
  Cons& operator=(Cons&&) = default;

  Lisp_ptr car() const;
  Lisp_ptr cdr() const;
  void rplaca(Lisp_ptr);
  void rplacd(Lisp_ptr);

private:
  Lisp_ptr car_;
  Lisp_ptr cdr_;
};

#include "cons.i.hh"

#endif // CONS_HH
