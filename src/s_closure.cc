#include "s_closure.hh"
#include "cons_util.hh"
#include "zs_error.hh"

SyntacticClosure::SyntacticClosure(Env* e, Cons* f, Lisp_ptr ex)
  : env_(e), free_names_(f), expr_(ex){
  for(auto i : Lisp_ptr{f}){
    if(!identifierp(i)){
      throw zs_error("syntactic closure error: free-list has a non-identifier value\n");
    }
  }
}

SyntacticClosure::~SyntacticClosure() = default;

// checks recursively
bool SyntacticClosure::is_alias() const{
  auto tag = expr_.tag();

  if(tag == Ptr_tag::symbol){
    return true;
  }else if(tag == Ptr_tag::syntactic_closure){
    return expr_.get<SyntacticClosure*>()->is_alias();
  }else{
    return false;
  }
}

bool identifierp(Lisp_ptr p){
  if(p.tag() == Ptr_tag::symbol){
    return true;
  }else if(p.tag() == Ptr_tag::syntactic_closure){
    return p.get<SyntacticClosure*>()->is_alias();
  }else{
    return false;
  }
}

Symbol* identifier_symbol(Lisp_ptr p){
  if(!identifierp(p)){
    throw zs_error("eval internal error: not identifier value! (%s)",
                   stringify(p.tag()));
  }

  if(p.tag() == Ptr_tag::symbol){
    return p.get<Symbol*>();
  }else if(p.tag() == Ptr_tag::syntactic_closure){
    auto sc = p.get<SyntacticClosure*>();
    assert(sc->is_alias());
    return sc->expr().get<Symbol*>();
  }else{
    UNEXP_DEFAULT();
  }
}
