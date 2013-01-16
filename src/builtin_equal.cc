#include <algorithm>

#include "builtin_equal.hh"
#include "util.hh"
#include "lisp_ptr.hh"
#include "number.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "vm.hh"
#include "cons.hh"

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
  case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::i_procedure:
  case Ptr_tag::n_procedure:
  case Ptr_tag::continuation:
  case Ptr_tag::number:
  case Ptr_tag::string:
  case Ptr_tag::vector:
  case Ptr_tag::input_port:
  case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::delay:
  case Ptr_tag::syntactic_closure:
  case Ptr_tag::syntax_rules:
  case Ptr_tag::vm_op:
    return a.get<void*>() == b.get<void*>();
  case Ptr_tag::vm_argcount:
    return a.get<int>() == b.get<int>();
  default:
    UNEXP_DEFAULT();
  }
}

bool eqv_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() == Ptr_tag::number && b.tag() == Ptr_tag::number){
    return eqv(*a.get<Number*>(), *b.get<Number*>());
  }else{
    return eq_internal(a, b);
  }
}

bool equal_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() == Ptr_tag::cons && b.tag() == Ptr_tag::cons){
    return do_list_2(a, b,
                     [](Cons* c1, Cons* c2){
                       return equal_internal(c1->car(), c2->car());
                     },
                     [](Lisp_ptr a_last, Lisp_ptr b_last){
                       return nullp(a_last) && nullp(b_last);
                     });
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

Lisp_ptr eq(){
  ZsArgs args{2};
  return Lisp_ptr{eq_internal(args[0], args[1])};
}

Lisp_ptr eqv(){
  ZsArgs args{2};
  return Lisp_ptr{eqv_internal(args[0], args[1])};
}

Lisp_ptr equal(){
  ZsArgs args{2};
  return Lisp_ptr{equal_internal(args[0], args[1])};
}
