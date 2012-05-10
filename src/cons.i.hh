#ifndef CONS_I_HH
#define CONS_I_HH

#ifndef CONS_HH
#error "Please include via parent file"
#endif

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

inline
bool nullp(Lisp_ptr p){
  return (p.tag() == Ptr_tag::cons)
    && (p.get<Cons*>() == Cons::NIL.get<Cons*>());
}

template<typename MainFun, typename LastFun>
auto do_list(Lisp_ptr lis, MainFun&& m_fun, LastFun&& l_fun)
  -> decltype(l_fun(lis)){
  Lisp_ptr p = lis;

  while(1){
    if(p.tag() != Ptr_tag::cons)
      break; // null or dot list end.

    auto c = p.get<Cons*>();
    if(!c) break; // reached nil

    if(!m_fun(c))
      break;

    p = c->cdr();
  }

  return l_fun(p);
}


#endif // CONS_I_HH
