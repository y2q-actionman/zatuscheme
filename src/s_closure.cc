#include "s_closure.hh"
#include "cons_util.hh"
#include "zs_error.hh"
#include "env.hh"

SyntacticClosure::SyntacticClosure(Env* e, Lisp_ptr f, Lisp_ptr ex)
  : env_(e), free_names_(f), expr_(ex){
  for(auto i : f){
    if(!identifierp(i)){
      throw zs_error(i, "syntactic closure: free-list has a non-identifier value");
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

bool identifier_eq(Env* ident1_env, Lisp_ptr ident1,
                   Env* ident2_env, Lisp_ptr ident2){
  return eq_internal(ident1_env->find(ident1), ident2_env->find(ident2));
}
