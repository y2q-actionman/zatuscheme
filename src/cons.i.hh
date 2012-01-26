#ifndef CONS_I_HH
#define CONS_I_HH

#ifndef CONS_HH
#error "Please include via parent file"
#endif

inline
Cons::Cons(Lisp_ptr x)
  : car_(x), cdr_(Cons::NIL){}

inline
Cons::Cons(Lisp_ptr x, Lisp_ptr y)
  : car_(x), cdr_(y){}

inline
Lisp_ptr Cons::car() const {
  return this ? car_ : NIL;
}

inline
Lisp_ptr Cons::cdr() const {
  return this ? cdr_ : NIL;
}

inline
void Cons::rplaca(Lisp_ptr p){
  if(this)
    car_ = p;
}

inline
void Cons::rplacd(Lisp_ptr p){
  if(this)
    cdr_ = p;
}

#endif // CONS_I_HH
