#include "s_closure.hh"
#include "cons_util.hh"
#include "zs_error.hh"
#include "env.hh"

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
bool identifierp(Lisp_ptr p){
  if(p.tag() == Ptr_tag::symbol){
    return true;
  }else if(p.tag() == Ptr_tag::syntactic_closure){
    return identifierp(p.get<SyntacticClosure*>()->expr());
  }else{
    return false;
  }
}

Symbol* identifier_symbol(Lisp_ptr p){
  if(p.tag() == Ptr_tag::symbol){
    return p.get<Symbol*>();
  }else if(p.tag() == Ptr_tag::syntactic_closure){
    return identifier_symbol(p.get<SyntacticClosure*>()->expr());
  }else{
    throw zs_error("eval internal error: not identifier! (%s)",
                   stringify(p.tag()));
  }
}

Env* identifier_env(Lisp_ptr p, Env* e){
  if(p.tag() == Ptr_tag::symbol){
    return e;
  }else if(p.tag() == Ptr_tag::syntactic_closure){
    auto sc = p.get<SyntacticClosure*>();
    return identifier_env(sc->expr(), sc->env());
  }else{
    throw zs_error("eval internal error: not identifier! (%s)",
                   stringify(p.tag()));
  }
}

bool identifier_eq(Env* ident1_env, Lisp_ptr ident1,
                   Env* ident2_env, Lisp_ptr ident2){
  return (ident1_env->find(ident1) == ident2_env->find(ident2));
}
