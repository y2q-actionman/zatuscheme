#include <cassert>

#include "procedure.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "util.hh"
#include "s_closure.hh"
#include "s_rules.hh"

using namespace ProcFlag;

std::pair<int, Variadic> parse_func_arg(Lisp_ptr args){
  int argc = 0;
  auto v = Variadic::f;

  do_list(args,
          [&](Cons* c) -> bool {
            if(!identifierp(c->car())){
              return false;
            }
            ++argc;
            return true;
          },
          [&](Lisp_ptr last){
            if(nullp(last)){
              return;
            }else{
              if(!identifierp(last)){
                throw zs_error_arg1("eval error", "informal lambda list!", {args});
              }
              v = Variadic::t;
            }
          });

  return {argc, v};
}

  // Continuation class

constexpr ProcInfo Continuation::cont_procinfo;

Continuation::Continuation(const VM& v) : vm_(v){}

Continuation::~Continuation() = default;


const ProcInfo* get_procinfo(Lisp_ptr p){
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
  case Ptr_tag::syntax_rules: {
    auto srule = p.get<SyntaxRules*>();
    assert(srule);

    return srule->info();
  }

  case Ptr_tag::undefined: case Ptr_tag::boolean:
  case Ptr_tag::character: case Ptr_tag::cons:
  case Ptr_tag::symbol:    case Ptr_tag::number:
  case Ptr_tag::string:    case Ptr_tag::vector:
  case Ptr_tag::input_port: case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::delay:
  case Ptr_tag::syntactic_closure:
  case Ptr_tag::vm_op:
  case Ptr_tag::vm_argcount:
    return nullptr;

  default:
    UNEXP_DEFAULT();
  }
}
