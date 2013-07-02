#include <algorithm>

#include "cons.hh"
#include "cons_util.hh"
#include "equality.hh"
#include "rational.hh"
#include "zs_error.hh"

using namespace std;

bool eq_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;

  switch(a.tag()){
  case Ptr_tag::undefined:
    return true;
  case Ptr_tag::boolean:
    return a.get<bool>() == b.get<bool>();
  case Ptr_tag::character:
     // this can be moved into eqv? in R5RS, but char is contained in Lisp_ptr.
    return a.get<char>() == b.get<char>();
  case Ptr_tag::integer:
  case Ptr_tag::vm_argcount:
    return a.get<int>() == b.get<int>();
  case Ptr_tag::notation:
    return a.get<Notation>() == b.get<Notation>();
  case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::i_procedure:
  case Ptr_tag::n_procedure:
  case Ptr_tag::continuation:
  case Ptr_tag::rational:
  case Ptr_tag::real:
  case Ptr_tag::complex:
  case Ptr_tag::string:
  case Ptr_tag::vector:
  case Ptr_tag::input_port:
  case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::syntactic_closure:
  case Ptr_tag::syntax_rules:
  case Ptr_tag::vm_op:
    return a.get<void*>() == b.get<void*>();
  default:
    UNEXP_DEFAULT();
  }
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
  if(a.tag() == Ptr_tag::cons && b.tag() == Ptr_tag::cons){
    auto i_a = begin(a);
    auto i_b = begin(b);

    for(; i_a && i_b; ++i_a, ++i_b){
      if(!equal_internal(*i_a, *i_b)) return false;
    }

    return (nullp(i_a.base()) && nullp(i_b.base()))
      || equal_internal(i_a.base(), i_b.base());
  }else if(a.tag() == Ptr_tag::vector && b.tag() == Ptr_tag::vector){
    auto v1 = a.get<Vector*>(), v2 = b.get<Vector*>();
    return std::equal(v1->begin(), v1->end(), v2->begin(), equal_internal);
  }else if(a.tag() == Ptr_tag::string && b.tag() == Ptr_tag::string){
    auto s1 = a.get<String*>(), s2 = b.get<String*>();
    return *s1 == *s2;
  }else{
    return eqv_internal(a, b);
  }
}

size_t eq_hash(Lisp_ptr p){
  auto tag_hash = hash<int>()(static_cast<int>(p.tag()));
  size_t val_hash;

  switch(p.tag()){
  case Ptr_tag::undefined:
    val_hash = hash<void*>()(nullptr);
    break;
  case Ptr_tag::boolean:
    val_hash = hash<bool>()(p.get<bool>());
    break;
  case Ptr_tag::character:
    val_hash = hash<char>()(p.get<char>());
    break;
  case Ptr_tag::integer:
  case Ptr_tag::vm_argcount:
    val_hash = hash<int>()(p.get<int>());
    break;
  case Ptr_tag::notation:
    val_hash = hash<int>()(static_cast<int>(p.get<Notation>()));
    break;
  case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::i_procedure:
  case Ptr_tag::n_procedure:
  case Ptr_tag::continuation:
  case Ptr_tag::rational:
  case Ptr_tag::real:
  case Ptr_tag::complex:
  case Ptr_tag::string:
  case Ptr_tag::vector:
  case Ptr_tag::input_port:
  case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::syntactic_closure:
  case Ptr_tag::syntax_rules:
  case Ptr_tag::vm_op:
    val_hash = hash<void*>()(p.get<void*>());
    break;
  default:
    UNEXP_DEFAULT();
  }

  return tag_hash ^ val_hash;
}
