#include <cassert>

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

constexpr ProcInfo Procedure::Continuation::cont_procinfo;

const ProcInfo* Procedure::get_procinfo(Lisp_ptr p){
  switch(p.tag()){
  case Ptr_tag::i_procedure: {
    auto iproc = p.get<IProcedure*>();
    assert(iproc);

    return iproc->info();
  }
  case Ptr_tag::n_procedure: {
    auto nproc = p.get<const NProcedure*>();
    assert(nproc);

    return nproc->info();
  }
  case Ptr_tag::continuation: {
    auto cont = p.get<Continuation*>();
    assert(cont);

    return cont->info();
  }

  case Ptr_tag::undefined: case Ptr_tag::boolean:
  case Ptr_tag::character: case Ptr_tag::cons:
  case Ptr_tag::symbol:    case Ptr_tag::number:
  case Ptr_tag::string:    case Ptr_tag::vector:
  case Ptr_tag::port:      case Ptr_tag::env:
  case Ptr_tag::delay:     case Ptr_tag::vm_op:
    return nullptr;

  default:
    UNEXP_DEFAULT();
  }
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
