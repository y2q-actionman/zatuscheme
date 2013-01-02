#include "s_closure.hh"

SyntacticClosure::SyntacticClosure(Env* e, Cons* f, Lisp_ptr ex)
  : env_(e), free_names_(f), expr_(ex){}

SyntacticClosure::~SyntacticClosure() = default;

bool identifierp(Lisp_ptr p){
  if(p.tag() == Ptr_tag::symbol){
    return true;
  }else if(p.tag() == Ptr_tag::syntactic_closure){
    return p.get<SyntacticClosure*>()->is_alias();
  }else{
    return false;
  }
}

