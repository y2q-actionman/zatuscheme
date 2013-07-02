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

std::pair<int, Variadic> parse_func_arg(Lisp_ptr args){
  if(identifierp(args)){
    return {0, Variadic::t};
  }

  int argc = 0;

  auto i = begin(args);
  for(; i; ++i){
    if(!identifierp(*i)){
      throw_zs_error(args, "eval error: informal lambda list!");
    }
    ++argc;
  }

  if(nullp(i.base())){
    return {argc, Variadic::f};
  }else{
    if(!identifierp(i.base())){
      throw_zs_error(args, "eval error: informal lambda list!");
    }
    return {argc, Variadic::t};
  }
}

  // Continuation class

constexpr ProcInfo Continuation::cont_procinfo;

Continuation::Continuation(const VM& v)
  : vm_(v), name_(){}

Continuation::~Continuation() = default;


const ProcInfo* get_procinfo(Lisp_ptr p){
  if(!is_procedure(p.tag())) return nullptr;

  if(p.tag() == Ptr_tag::i_procedure){
    auto iproc = p.get<IProcedure*>();
    assert(iproc);
    return iproc->info();
  }else if(p.tag() == Ptr_tag::n_procedure){
    auto nproc = p.get<const NProcedure*>();
    assert(nproc);
    return nproc->info();
  }else if(p.tag() == Ptr_tag::continuation){
    auto cont = p.get<Continuation*>();
    assert(cont);
    return cont->info();
  }else if(p.tag() == Ptr_tag::syntax_rules){
    auto srule = p.get<SyntaxRules*>();
    assert(srule);
    return srule->info();
  }else{
    UNEXP_DEFAULT();
  }
}

Lisp_ptr get_procname(Lisp_ptr p){
  if(p.tag() == Ptr_tag::i_procedure){
    auto iproc = p.get<IProcedure*>();
    assert(iproc);
    return iproc->name();
  }else if(p.tag() == Ptr_tag::n_procedure){
    auto nproc = p.get<const NProcedure*>();
    assert(nproc);
    return zs_new<String>(find_builtin_nproc_name(nproc));
  }else if(p.tag() == Ptr_tag::continuation){
    auto cont = p.get<Continuation*>();
    assert(cont);
    return cont->name();
  }else if(p.tag() == Ptr_tag::syntax_rules){
    auto srule = p.get<SyntaxRules*>();
    assert(srule);
    return srule->name();
  }else{
    UNEXP_DEFAULT();
  }
}

void set_procname(Lisp_ptr p, Lisp_ptr n){
  if(p.tag() == Ptr_tag::i_procedure){
    auto iproc = p.get<IProcedure*>();
    assert(iproc);
    iproc->set_name(n);
  }else if(p.tag() == Ptr_tag::n_procedure){
    return;
  }else if(p.tag() == Ptr_tag::continuation){
    auto cont = p.get<Continuation*>();
    assert(cont);
    cont->set_name(n);
  }else if(p.tag() == Ptr_tag::syntax_rules){
    auto srule = p.get<SyntaxRules*>();
    assert(srule);
    srule->set_name(n);
  }else{
    UNEXP_DEFAULT();
  }
}
