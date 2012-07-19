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
  // When Lisp_ptr::get<Cons*>() became constexpr,
  // '<void*>' should be replaced with '<Cons*>'
  static_assert(Cons::NIL.get<void*>() == nullptr,
                "NIL's pointer part is not nullptr!");
  return (p.tag() == Ptr_tag::cons)
    && (p.get<void*>() == nullptr);
}

template<typename MainFun, typename LastFun>
auto do_list(Lisp_ptr lis, MainFun&& m_fun, LastFun&& l_fun)
  -> decltype(l_fun(lis)){
  Lisp_ptr p = lis;

  while(auto c = p.get<Cons*>()){
    auto next = c->cdr();
    if(!m_fun(c))
      break;

    p = next;
  }

  return l_fun(p);
}

template<int len, typename Fun1, typename... FunRest>
inline
int bind_cons_list_i(Lisp_ptr p, Fun1&& f, FunRest&&... fr){
  auto c = p.get<Cons*>();
  if(!c) return len;

  auto next = c->cdr();
  f(c);

  return bind_cons_list_i<len + 1>(next, fr...);
}

template<int len>
inline
int bind_cons_list_i(Lisp_ptr p){
  return (nullp(p)) ? len : len+1;
}

template<typename... Fun>
inline
int bind_cons_list(Lisp_ptr p, Fun&&... f){
  return bind_cons_list_i<0>(p, f...);
}

#endif // CONS_I_HH
