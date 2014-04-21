#include <cassert>
#include <string>

#include "builtin.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "procedure.hh"
#include "s_closure.hh"
#include "s_rules.hh"
#include "zs_error.hh"

using namespace proc_flag;

std::pair<ProcInfo::ArgsType, Variadic> parse_func_arg(Lisp_ptr args){
  ProcInfo::ArgsType argc = 0;

  auto i = begin(args);
  for(; i; ++i){
    check_identifier_type(*i);
    ++argc;
  }

  if(nullp(i.base())){
    return {argc, Variadic::f};
  }else{
    check_identifier_type(i.base());
    return {argc, Variadic::t};
  }
}

// Continuation class
static constexpr ProcInfo cont_procinfo = ProcInfo{0, proc_flag::Variadic::t};

template<typename IFun, typename NFun, typename CFun, typename SFun, typename DFun>
auto access_proc(Lisp_ptr p, IFun ifun, NFun nfun, CFun cfun, SFun sfun, DFun dfun)
  -> decltype(ifun(nullptr)){
  switch(p.tag()){
  case Ptr_tag::i_procedure: {
    auto iproc = p.get<IProcedure*>();
    assert(iproc);
    return ifun(iproc);
  }
  case Ptr_tag::n_procedure: {
    auto nproc = p.get<const NProcedure*>();
    assert(nproc);
    return nfun(nproc);
  }
  case Ptr_tag::continuation: {
    auto cont = p.get<VM*>();
    assert(cont);
    return cfun(cont);
  }
  case Ptr_tag::syntax_rules: {
    auto srule = p.get<SyntaxRules*>();
    assert(srule);
    return sfun(srule);
  }
  case Ptr_tag::undefined: case Ptr_tag::boolean:
  case Ptr_tag::character: case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::integer: case Ptr_tag::rational:
  case Ptr_tag::real:    case Ptr_tag::complex:
  case Ptr_tag::string:    case Ptr_tag::vector:
  case Ptr_tag::input_port: case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::syntactic_closure:
  case Ptr_tag::vm_op:
  case Ptr_tag::vm_argcount:
  case Ptr_tag::notation:
    return dfun();
  default:
    UNEXP_DEFAULT();
  }
}

bool is_procedure(Lisp_ptr p){
  return access_proc(p,
                     [](IProcedure*){ return true; },
                     [](const NProcedure*){ return true; },
                     [](VM*){ return true; },
                     [](SyntaxRules*){ return true; },
                     [](){ return false; });
}

const ProcInfo* get_procinfo(Lisp_ptr p){
  return access_proc(p,
                     [](IProcedure* iproc){ return iproc->info(); },
                     [](const NProcedure* nproc){ return nproc->info(); },
                     [](VM*){ return &cont_procinfo; },
                     [](SyntaxRules* srule){ return srule->info(); },
                     [](){ return nullptr; });
}

Lisp_ptr get_procname(Lisp_ptr p){
  return access_proc(p,
                     [](IProcedure* iproc){ return iproc->name(); },
                     [](const NProcedure* nproc){
                       return zs_new<String>(nproc->name());
                     },
                     [](VM* cont){ return cont->name; },
                     [](SyntaxRules* srule){ return srule->name(); },
                     []() -> Lisp_ptr { UNEXP_DEFAULT(); });
}

void set_procname(Lisp_ptr p, Lisp_ptr n){
  return access_proc(p,
                     [&](IProcedure* iproc){ iproc->set_name(n); },
                     [](const NProcedure*){},
                     [&](VM* cont){ cont->name = n; },
                     [&](SyntaxRules* srule){ srule->set_name(n); },
                     [](){ UNEXP_DEFAULT(); });
}
