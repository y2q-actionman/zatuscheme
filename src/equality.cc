#include <algorithm>

#include "cons.hh"
#include "cons_util.hh"
#include "equality.hh"
#include "rational.hh"
#include "zs_error.hh"

using namespace std;

bool eq_internal(Lisp_ptr a, Lisp_ptr b){
  return (a.tag() == b.tag()
          && a.get<void*>() == b.get<void*>());
}

bool eqv_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;
  
  if(a.tag() == Ptr_tag::rational){
    return *a.get<Rational*>() == *b.get<Rational*>();
  }else if(a.tag() == Ptr_tag::real){
    return *a.get<double*>() == *b.get<double*>();
  }else if(a.tag() == Ptr_tag::complex){
    return *a.get<Complex*>() == *b.get<Complex*>();
  }else{
    return eq_internal(a, b);
  }
}

bool equal_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;
  
  if(a.tag() == Ptr_tag::cons){
    auto i_a = begin(a);
    auto i_b = begin(b);

    for(; i_a && i_b; ++i_a, ++i_b){
      if(!equal_internal(*i_a, *i_b)) return false;
    }

    return (nullp(i_a.base()) && nullp(i_b.base()))
      || equal_internal(i_a.base(), i_b.base());
  }else if(a.tag() == Ptr_tag::vector){
    auto v1 = a.get<Vector*>(), v2 = b.get<Vector*>();
    return std::equal(v1->begin(), v1->end(), v2->begin(), equal_internal);
  }else if(a.tag() == Ptr_tag::string){
    auto s1 = a.get<String*>(), s2 = b.get<String*>();
    return *s1 == *s2;
  }else{
    return eqv_internal(a, b);
  }
}
