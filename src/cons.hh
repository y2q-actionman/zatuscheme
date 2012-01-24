#ifndef CONS_HH
#define CONS_HH

#include "lisp_ptr.hh"

struct Cons {
  Lisp_ptr car_;
  Lisp_ptr cdr_;
};

#endif // CONS_HH
