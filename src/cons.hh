#ifndef CONS_HH
#define CONS_HH

#include "lisp_ptr.hh"

class Cons {
public:
  static const Lisp_ptr NIL;

  Cons() = delete;
  explicit Cons(Lisp_ptr x) // one element list
    : car_(x), cdr_(NIL){}
  Cons(Lisp_ptr x, Lisp_ptr y)
    : car_(x), cdr_(y){}
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
