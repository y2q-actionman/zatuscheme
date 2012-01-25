#ifndef CONS_HH
#define CONS_HH

#include "lisp_ptr.hh"

struct Cons {
  Lisp_ptr car_;
  Lisp_ptr cdr_;
};

inline
Lisp_ptr cons(Lisp_ptr x, Lisp_ptr y){
  Cons c{x, y};
  return Lisp_ptr{ new Cons{c} };
}

#endif // CONS_HH
