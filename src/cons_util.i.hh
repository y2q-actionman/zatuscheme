#ifndef CONS_UTIL_I_HH
#define CONS_UTIL_I_HH

#ifndef CONS_UTIL_HH
#error "Please include via parent file"
#endif

#include "equality.hh"
#include "zs_error.hh"
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

inline
bool is_nonnull_cons(Lisp_ptr p){
  return (p.tag() == Ptr_tag::cons)
    && (p.get<void*>() != nullptr);
}

// make_cons_list 
template<typename Iter>
Lisp_ptr make_cons_list(Iter b, Iter e){
  if(b == e){
    return Cons::NIL;
  }

  GrowList gw;

  do{
    gw.push(Lisp_ptr{*b});
  }while(++b != e);

  return gw.extract();
}


// nth family
template<unsigned n>
Lisp_ptr nth_cons_list(Lisp_ptr p){
  check_nonnull_cons(p);
  // This cast is required for telling a type to the compiler (g++-4.6). 
  return static_cast<Lisp_ptr>(nthcdr_cons_list<n>(p)).get<Cons*>()->car;
}

template<>
inline
Lisp_ptr nthcdr_cons_list<0u>(Lisp_ptr p){
  return p;
}

template<unsigned n>
Lisp_ptr nthcdr_cons_list(Lisp_ptr p){
  check_nonnull_cons(p);
  return nthcdr_cons_list<n-1>(p.get<Cons*>()->cdr);
}


// GrowList class
inline
Lisp_ptr GrowList::extract(){
  return extract_with_tail(Cons::NIL);
}

#endif //CONS_UTIL_I_HH
