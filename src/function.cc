#include "function.hh"
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
              return {args, argc, false};
            }else{
              if(last.tag() != Ptr_tag::symbol){
                fprintf(stderr, "eval error: informal lambda list! (including non-symbol)\n");
                return {};
              }
              return {args, argc, true};
            }
          });
}

const char* stringify(Function::Type t){
  switch(t){
  case Function::Type::interpreted:
    return "interpreted";
  case Function::Type::native:
    return "native";
  case Function::Type::interpreted_macro:
    return "interpreted macro";
  case Function::Type::native_macro:
    return "native macro";
  default:
    return "(unknown function type)";
  }
}

void describe(FILE* f, const Function::ArgInfo& argi){
  fprintf(f, "[code=");
  describe(f, argi.head);
  fprintf(f, ", required_args=%d, variadic=%d]",
          argi.required_args, argi.variadic);
}
