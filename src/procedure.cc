#include "procedure.hh"
#include "cons.hh"

Function::ArgInfo parse_func_arg(Lisp_ptr args){
  int argc = 0;

  return
  do_list(args,
          [&](Cons* c) -> bool {
            if(c->car().tag() != Ptr_tag::symbol){
              return false;
            }
            ++argc;
            return true;
          },
          [&](Lisp_ptr last) -> Function::ArgInfo {
            if(nullp(last)){
              return {argc, false, args};
            }else{
              if(last.tag() != Ptr_tag::symbol){
                fprintf(stderr, "eval error: informal lambda list! (including non-symbol)\n");
                return {};
              }
              return {argc, true, args};
            }
          });
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

const char* stringify(Function::Calling c){
  switch(c){
  case Function::Calling::function:
    return "function";
  case Function::Calling::macro:
    return "macro";
  case Function::Calling::whole_function:
    return "whole_function";
  case Function::Calling::whole_macro:
    return "whole_macro";
  default:
    return "(unknown calling type)";
  }
}

void describe(FILE* f, const Function::ArgInfo& argi){
  fprintf(f, "[code=");
  describe(f, argi.head);
  fprintf(f, ", required_args=%d, variadic=%d]",
          argi.required_args, argi.variadic);
}
