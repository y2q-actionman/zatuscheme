#ifndef CONS_UTIL_I_HH
#define CONS_UTIL_I_HH

#ifndef CONS_UTIL_HH
#error "Please include via parent file"
#endif

#include <utility>
#include <algorithm>
#include <type_traits>
#include "zs_error.hh"
#include "equality.hh"
#include "zs_memory.hh"

inline
bool nullp(Lisp_ptr p){
  // When Lisp_ptr::get<Cons*>() became constexpr,
  // '<void*>' should be replaced with '<Cons*>'
  static_assert(Cons::NIL.get<void*>() == nullptr,
                "NIL's pointer part is not nullptr!");
  return (p.tag() == Ptr_tag::cons)
    && (p.get<void*>() == nullptr);
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
  return Lisp_ptr(zs_new<Cons>(p, q));
}


// nth family
template<unsigned n>
constexpr
Lisp_ptr nth_cons_list(Lisp_ptr p){
  // This cast is for telling a type to the compiler. 
  return car(static_cast<Lisp_ptr>(nthcdr_cons_list<n>(p)).get<Cons*>());
}

template<>
constexpr
Lisp_ptr nthcdr_cons_list<0u>(Lisp_ptr p){
  return p;
}

template<unsigned n>
constexpr
Lisp_ptr nthcdr_cons_list(Lisp_ptr p){
  return nthcdr_cons_list<n-1>(cdr(p.get<Cons*>()));
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
bool operator==(const ConsIter& i1, const ConsIter& i2){
  return eq_internal(i1.base(), i2.base());
}

inline
bool operator!=(const ConsIter& i1, const ConsIter& i2){
  return !eq_internal(i1.base(), i2.base());
}

#endif //CONS_UTIL_I_HH
