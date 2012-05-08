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

template<typename MainFun, typename ErrFun>
int do_list(Lisp_ptr l, MainFun&& m_fun, ErrFun&& e_fun){
  if(l.tag() != Ptr_tag::cons){
    e_fun(l);
    return -1;
  }    

  int ret = 0;
  Lisp_ptr next;

  for(auto c = l.get<Cons*>(); c; c = next.get<Cons*>()){
    next = c->cdr();

    if(!m_fun(c->car(), next))
      break;
    ++ret;

    if(next.tag() != Ptr_tag::cons){
      e_fun(next);
      break;
    }
  }

  return ret;
}


#endif // CONS_I_HH
