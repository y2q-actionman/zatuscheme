#include "builtin_extra.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "s_closure.hh"
#include "util.hh"
#include "eval.hh"
#include "env.hh"
#include "equality.hh"
#include "number.hh"
#include "builtin.hh"
#include "zs_error.hh"
#include "cons_util.hh"

using namespace std;
using namespace ProcFlag;

Lisp_ptr traditional_transformer(){
  ZsArgs args;

  auto iproc = args[0].get<IProcedure*>();
  if(!iproc){
    throw zs_error_arg1("traditional-transformer", "called with a wrong type", {args[0]});
  }
  auto info = *iproc->info();
  info.passing = Passing::quote;
  info.returning = Returning::code;

  return new IProcedure(iproc->get(), info,
                        iproc->arg_list(), iproc->closure());
}

Lisp_ptr gensym(){
  static const string gensym_symname = {"(gensym)"};
  ZsArgs args;
  return {new Symbol(&gensym_symname)};
}

Lisp_ptr sc_macro_transformer(){
  ZsArgs args;

  auto iproc = args[0].get<IProcedure*>();
  if(!iproc){
    throw zs_error_arg1("sc-macro-transformer", "called with a wrong type", {args[0]});
  }

  auto info = *iproc->info();
  if(info.required_args != 2 || info.max_args != 2){
    throw zs_error_arg1("sc-macro-transformer",
                        printf_string("procedure must take exactly 2 args (this takes %d-%d))",
                                      info.required_args, info.max_args));
  }

  info.passing = Passing::whole;
  info.returning = Returning::code;
  info.leaving = Leaving::after_returning_op;

  return new IProcedure(iproc->get(), info,
                        iproc->arg_list(),
                        iproc->closure()->fork());
}

Lisp_ptr make_syntactic_closure(){
  ZsArgs args;

  Env* e = args[0].get<Env*>();
  if(!e){
    throw builtin_type_check_failed("make-syntactic-closure",
                                    Ptr_tag::env, args[0]);
  }

  if(args[1].tag() != Ptr_tag::cons){
    throw builtin_type_check_failed("make-syntactic-closure",
                                    Ptr_tag::cons, args[1]);
  }
  Cons* c = args[1].get<Cons*>();

  return new SyntacticClosure(e, c, args[2]);
}

Lisp_ptr capture_env(){
  ZsArgs args;

  if(args[0].tag() != Ptr_tag::i_procedure){
    throw zs_error_arg1("capture-syntactic-environment", "first arg is not procedure", {args[0]});
  }

  auto iproc = args[0].get<IProcedure*>();

  assert(iproc && iproc->info());

  if(iproc->info()->required_args != 1){
    throw zs_error_arg1("capture-syntactic-environment",
                        printf_string("first arg mush take 1 arg (take %d)",
                                      iproc->info()->required_args));
  }

  auto ret =  make_cons_list
    ({find_builtin_nproc("eval"),
        make_cons_list({find_builtin_nproc("apply"), iproc, vm_op_get_current_env}),
        vm_op_get_current_env});

  return ret;
}

Lisp_ptr proc_identifierp(){
  ZsArgs args;
  return Lisp_ptr{identifierp(args[0])};
}

Lisp_ptr proc_identifier_eq(){
  ZsArgs args;

  auto ident1_env = args[0].get<Env*>();
  if(!ident1_env){
    throw builtin_type_check_failed("identifier=?", Ptr_tag::env, args[0]);
  }

  if(!identifierp(args[1])){
    throw builtin_identifier_check_failed("identifier=?", args[1]);
  }
  
  auto ident2_env = args[2].get<Env*>();
  if(!ident2_env){
    throw builtin_type_check_failed("identifier=?", Ptr_tag::env, args[2]);
  }

  if(!identifierp(args[3])){
    throw builtin_identifier_check_failed("identifier=?", args[3]);
  }

  return 
    Lisp_ptr{identifier_eq(ident1_env, args[1], ident2_env, args[3])};
}

Lisp_ptr make_synthetic_identifier(){
  ZsArgs args;

  if(!identifierp(args[0])){
    throw zs_error_arg1("make-synthetic-identifier", "passed value is not identifier", {args[0]});
  }

  return new SyntacticClosure(new Env(nullptr), nullptr, args[0]);
}

Lisp_ptr exit_func(){
  {
    ZsArgs args; 
  }
  // cerr << "exiting..\n";
  vm.stack.clear();
  vm.code.clear();
  return {};
}

Lisp_ptr transcript_on(){
  ZsArgs args; 
  dump_mode = true;
  return Lisp_ptr{true};
}

Lisp_ptr transcript_off(){
  ZsArgs args; 
  dump_mode = false;
  return Lisp_ptr{true};
}
