#include "procedure.hh"
#include "cons.hh"
#include "util.hh"

using namespace Procedure;

std::pair<int, Variadic> Procedure::parse_func_arg(Lisp_ptr args){
  int argc = 0;
  auto v = Variadic::f;

  do_list(args,
          [&](Cons* c) -> bool {
            if(c->car().tag() != Ptr_tag::symbol){
              return false;
            }
            ++argc;
            return true;
          },
          [&](Lisp_ptr last){
            if(nullp(last)){
              return;
            }else{
              if(last.tag() != Ptr_tag::symbol){
                fprintf(zs::err, "eval error: informal lambda list! (including non-symbol)\n");
                argc = -1;
                return;
              }
              v = Variadic::t;
            }
          });

  return {argc, v};
}

//constexpr ProcInfo Procedure::Continuation::cont_procinfo{Calling::function, 1, Variadic::t};

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
