#ifndef CONS_UTIL_I_HH
#define CONS_UTIL_I_HH

#ifndef CONS_UTIL_HH
#error "Please include via parent file"
#endif

#include <utility>

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


// GrowList class
inline
void GrowList::invalidate(){
  head = {};
  next = nullptr;
}  

inline
GrowList::GrowList()
  : head(Cons::NIL), next(&head)
{}

inline
GrowList::GrowList(GrowList&& other)
  : head(std::move(other.head)),
    next(std::move(other.next))
{
  other.invalidate();
}

inline
GrowList::~GrowList(){
  if(auto c = head.get<Cons*>()){
    free_cons_list(c);
  }
  // invalidate();
}

inline
GrowList& GrowList::operator=(GrowList&& other){
  head = std::move(other.head);
  next = std::move(other.next);

  other.invalidate();

  return *this;
}

inline
Lisp_ptr GrowList::extract(){
  auto ret = head;
  invalidate();
  return ret;
}

#endif //CONS_UTIL_I_HH