#include "hasher.hh"
#include "zs_error.hh"
#include "s_closure.hh"

using namespace std;

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
