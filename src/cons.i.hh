#ifndef CONS_I_HH
#define CONS_I_HH

#ifndef CONS_HH
#error "Please include via parent file"
#endif

#include <cassert>

constexpr inline
Cons::Cons(Lisp_ptr x, Lisp_ptr y)
  : car_(x), cdr_(y){}

inline
Lisp_ptr car(Cons* c){
  return c ? c->car_ : Cons::NIL;
}

inline
Lisp_ptr cdr(Cons* c){
  return c ? c->cdr_ : Cons::NIL;
}

inline
void rplaca(Cons* c, Lisp_ptr p){
  assert(c);
  c->car_ = p;
}

inline
void rplacd(Cons* c, Lisp_ptr p){
  assert(c);
  c->cdr_ = p;
}

#endif // CONS_I_HH
