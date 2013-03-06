#include <algorithm>

#include "equality.hh"
#include "util.hh"
#include "number.hh"
#include "printer.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "s_closure.hh"
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
  case Ptr_tag::integer:
  case Ptr_tag::vm_argcount:
    return a.get<int>() == b.get<int>();
  default:
    UNEXP_DEFAULT();
  }
}

bool eq_id_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() == Ptr_tag::syntactic_closure
     && b.tag() == Ptr_tag::syntactic_closure){
    auto sc_a = a.get<SyntacticClosure*>();
    auto sc_b = b.get<SyntacticClosure*>();
    return (sc_a->env() == sc_b->env()) && eq_internal(sc_a->expr(), sc_b->expr());
  }else{
    return eq_internal(a, b);
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

size_t eq_hash(const Lisp_ptr& p){
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
    val_hash = hash<void*>()(p.get<void*>());
    break;
  case Ptr_tag::integer:
  case Ptr_tag::vm_argcount:
    val_hash = hash<int>()(p.get<int>());
    break;
  default:
    UNEXP_DEFAULT();
  }

  return tag_hash ^ val_hash;
}

size_t eq_id_hash(const Lisp_ptr& p){
  if(p.tag() == Ptr_tag::syntactic_closure){
    auto sc = p.get<SyntacticClosure*>();
    return hash<void*>()(sc->env()) ^ eq_hash(sc->expr());
  }else{
    return eq_hash(p);
  }
}
