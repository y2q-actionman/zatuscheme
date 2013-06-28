#include <cassert>
#include <string>

#include "procedure.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "s_closure.hh"
#include "s_rules.hh"
#include "builtin.hh"

using namespace proc_flag;

std::pair<int, Variadic> parse_func_arg(Lisp_ptr args){
  if(identifierp(args)){
    return {0, Variadic::t};
  }

  int argc = 0;

  auto i = begin(args);
  for(; i; ++i){
    if(!identifierp(*i)){
      throw zs_error("eval error: informal lambda list!", args);
    }
    ++argc;
  }

  if(nullp(i.base())){
    return {argc, Variadic::f};
  }else{
    if(!identifierp(i.base())){
      throw zs_error("eval error: informal lambda list!", args);
    }
    return {argc, Variadic::t};
  }
}

  // Continuation class

constexpr ProcInfo Continuation::cont_procinfo;

Continuation::Continuation(const VM& v) : vm_(v){}

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
    return zs_new<String>("(continuation)");
  }else if(p.tag() == Ptr_tag::syntax_rules){
    return zs_new<String>("(syntax-rules)");
  }else{
    UNEXP_DEFAULT();
  }
}
