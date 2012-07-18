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
Lisp_ptr Cons::car() const {
  return this ? car_ : NIL;
}

inline
Lisp_ptr Cons::cdr() const {
  return this ? cdr_ : NIL;
}

inline
void Cons::rplaca(Lisp_ptr p){
  assert(this);
  car_ = p;
}

inline
void Cons::rplacd(Lisp_ptr p){
  assert(this);
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

  while(p.tag() == Ptr_tag::cons){
    auto c = p.get<Cons*>();
    if(!c) break; // reached nil

    if(!m_fun(c))
      break;

    p = c->cdr();
  }

  return l_fun(p);
}

template<typename Fun1, typename... FunRest>
int bind_cons_list_i(int len, Lisp_ptr p, Fun1&& f, FunRest&&... fr){
  auto c = p.get<Cons*>();
  if(!c) return len;

  len += 1;
  f(c);

  // if(nullp(c->cdr())) return len; // this test is included in the first test.

  return bind_cons_list_i(len, c->cdr(), fr...);
}

inline
int bind_cons_list_i(int len, Lisp_ptr p){
  return (nullp(p)) ? len : len+1;
}

template<typename... Fun>
inline
int bind_cons_list(Lisp_ptr p, Fun&&... f){
  return bind_cons_list_i(0, p, f...);
}

#endif // CONS_I_HH
