#include "procedure.hh"
#include "cons.hh"
#include "util.hh"

using namespace Procedure;

ArgInfo parse_func_arg(Lisp_ptr args){
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
          [&](Lisp_ptr last) -> ArgInfo {
            if(nullp(last)){
              return {argc, false, args};
            }else{
              if(last.tag() != Ptr_tag::symbol){
                fprintf(zs::err, "eval error: informal lambda list! (including non-symbol)\n");
                return {};
              }
              return {argc, true, args};
            }
          });
}

const char* stringify(Calling c){
  switch(c){
  case Calling::function:
    return "function";
  case Calling::macro:
    return "macro";
  case Calling::whole_function:
    return "whole_function";
  case Calling::whole_macro:
    return "whole_macro";
  default:
    return "(unknown calling type)";
  }
}

void describe(FILE* f, const ArgInfo& argi){
  fprintf(f, "[code=");
  describe(f, argi.head);
  fprintf(f, ", required_args=%d, variadic=%d]",
          argi.required_args, argi.variadic);
}
