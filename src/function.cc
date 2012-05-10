#include "function.hh"
#include "util.hh"
#include "cons.hh"

Function::ArgInfo parse_func_arg(Lisp_ptr args){
  int argc = 0;

  return
  do_list(args,
          [&](Cons* c) -> bool {
            if(c->car().tag() != Ptr_tag::symbol){
              fprintf(stderr, "eval error: informal lambda list! (includes non-symbol)\n");
              argc = -1;
              return false;
            }
            ++argc;
            return true;
          },
          [&](Lisp_ptr dot_cdr) -> Function::ArgInfo {
            if(argc < 0) return {};

            if(nullp(dot_cdr)){
              return {args, argc, false};
            }else{
              if(dot_cdr.tag() != Ptr_tag::symbol){
                fprintf(stderr, "eval error: informal lambda list! (ended with non-symbol)\n");
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
