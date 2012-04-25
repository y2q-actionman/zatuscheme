#include "function.hh"
#include "util.hh"
#include "cons.hh"

Function::ArgInfo parse_func_arg(Lisp_ptr args){
  int argc = 0;
  Lisp_ptr p = args;

  while(1){
    if(p.tag() != Ptr_tag::cons){
      if(p.tag() != Ptr_tag::symbol){
        fprintf(stderr, "eval error: informal lambda list! (ended with non-symbol)\n");
        return {};
      }

      return {args, argc, true};
    }

    Cons* c = p.get<Cons*>();
    if(!c){
      return {args, argc, false};
    }

    if(c->car().tag() != Ptr_tag::symbol){
      fprintf(stderr, "eval error: informal lambda list! (includes non-symbol)\n");
      return {};
    }

    ++argc;
    p = c->cdr();
  }
}

const char* stringify(Function::Type t){
  switch(t){
  case Function::Type::interpreted:
    return "interpreted";
  case Function::Type::native:
    return "native";
  default:
    return "(unknown function type)";
  }
}
