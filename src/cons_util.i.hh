#ifndef CONS_UTIL_I_HH
#define CONS_UTIL_I_HH

#ifndef CONS_UTIL_HH
#error "Please include via parent file"
#endif

#include <utility>
#include <algorithm>
#include "util.hh"

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

template<typename MainFun, typename LastFun>
auto do_list_2(Lisp_ptr lis1, Lisp_ptr lis2, MainFun&& m_fun, LastFun&& l_fun)
  -> decltype(l_fun(lis1, lis2)){
  Lisp_ptr p1 = lis1;
  Lisp_ptr p2 = lis2;

  Cons *c1, *c2;

  while((c1 = p1.get<Cons*>()) && (c2 = p2.get<Cons*>())){
    auto next1 = c1->cdr();
    auto next2 = c2->cdr();
    if(!m_fun(c1, c2))
      break;

    p1 = next1;
    p2 = next2;
  }

  return l_fun(p1, p2);
}

// bind_cons_list first version. uses number of functions
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

// bind_cons_list second version. uses std::function.
template<unsigned i>
struct destruct_cons_list{
  static constexpr auto expander = destruct_cons_list<i - 1>();

  template<typename Fun, typename... Args>
  auto operator()(Lisp_ptr p, Fun f, Args... args) const
    -> decltype(expander(p, f, args..., nullptr))
  {
    Cons* c = p.get<Cons*>();
    return expander(c->cdr(), f, args..., c);
  }
};

template<>
struct destruct_cons_list<0>{
  template<typename Fun, typename... Args>
  auto operator()(Lisp_ptr, Fun f, Args... args) const
    -> decltype(f(args...))
  {
    return f(args...);
  }
};

template<typename Ret, typename... Args>
Ret bind_cons_list(Lisp_ptr p, std::function<Ret (Args...)> fun){
  static constexpr destruct_cons_list<sizeof...(Args)> expander;
  return expander(p, fun);
}


// experimental third version
// http://stackoverflow.com/questions/6512019/can-we-get-the-type-of-a-lambda-argument
template<unsigned i, typename... F_Args>
struct typed_destruct;

template<unsigned i, typename F_Arg1, typename... F_Args>
struct typed_destruct<i, F_Arg1, F_Args...>{
  static constexpr auto expander = typed_destruct<i - 1, F_Args...>();

  template<typename Fun, typename... Args>
  auto operator()(Lisp_ptr p, Fun f, Args... args) const
    -> decltype(expander(p, f, args..., nullptr))
  {
    Cons* c = p.get<Cons*>();
    return expander(c->cdr(), f, args..., c);
  }

};

template<typename... F_Args>
struct typed_destruct<0, F_Args...>{
  template<typename Fun, typename... Args>
  auto operator()(Lisp_ptr, Fun f, Args... args) const
    -> decltype(f(args...))
  {
    return f(args...);
  }
};

template<typename Fun, typename Ret, typename... Args>
Ret entry_typed_destruct(Lisp_ptr p, Fun fun, Ret (Fun::*)(Args...)){
  return typed_destruct<sizeof...(Args), Args...>()(p, fun);
}
  
template<typename Fun, typename Ret, typename... Args>
Ret entry_typed_destruct(Lisp_ptr p, Fun fun, Ret (Fun::*)(Args...) const){
  return typed_destruct<sizeof...(Args), Args...>()(p, fun);
}
  
template<typename Fun, typename Ret, typename... Args>
Ret entry_typed_destruct(Lisp_ptr p, Fun fun, Ret (*)(Args...)){
  return typed_destruct<sizeof...(Args), Args...>()(p, fun);
}
  
template<typename Fun>
auto bind_cons_list_t(Lisp_ptr p, Fun fun)
  -> decltype(entry_typed_destruct(p, fun, &Fun::operator()))
{
  return entry_typed_destruct(p, fun, &Fun::operator());
}

  

// make_cons_list 
template<typename Iter>
Lisp_ptr make_cons_list(Iter b, Iter e){
  if(b == e){
    return Cons::NIL;
  }

  auto i = b;
  GrowList gw;

  while(1){
    gw.push(Lisp_ptr{*i});

    ++i;
    if(i == e) break;
  }

  return gw.extract();
}

inline
Lisp_ptr make_cons_list(std::initializer_list<Lisp_ptr> lis){
  return make_cons_list(begin(lis), end(lis));
}

inline
Lisp_ptr push_cons_list(Lisp_ptr p, Lisp_ptr q){
  return Lisp_ptr(new Cons(p, q));
}


// GrowList class
inline
void GrowList::invalidate(){
  assert(head && next);
  head = {};
  next = nullptr;
}  

inline
GrowList::GrowList()
  : head(Cons::NIL), next(&head)
{}

inline
Lisp_ptr GrowList::extract(){
  return extract_with_tail(Cons::NIL);
}

inline
Lisp_ptr GrowList::extract_with_tail(Lisp_ptr p){
  *next = p;
  auto ret = head;
  invalidate();
  return ret;
}


// ConsIter class

inline
ConsIter::ConsIter() : c_(nullptr){}

inline
ConsIter::ConsIter(const Cons* c) : c_(c){}

inline
ConsIter ConsIter::operator++(int){
  auto ret = *this;
  ++(*this);
  return ret;
}

inline
bool operator==(const ConsIter& i1, const ConsIter& i2){
  return i1.c_ == i2.c_;
}

inline
bool operator!=(const ConsIter& i1, const ConsIter& i2){
  return i1.c_ != i2.c_;
}

// cons_list_to_array
template<int size>
std::array<Lisp_ptr, size> cons_list_to_array(Lisp_ptr p){
  std::array<Lisp_ptr, size> ret;
  int i = 0;

  for(auto it = begin(p), e_it = end(p); it != e_it; ++it){
    if(i >= size)
      throw make_zs_error("passed list is longer than expected size (%d)\n", size);

    ret[i] = *it;
    ++i;
  }
  
  if(i != size)
    throw make_zs_error("passed list is shorter than expected size (%d)\n", size);

  return ret;
}

#endif //CONS_UTIL_I_HH
