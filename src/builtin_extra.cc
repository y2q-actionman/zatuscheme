#include "builtin_extra.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

Lisp_ptr to_macro_procedure(){
  ZsArgs args{1};

  auto proc = args[0].get<IProcedure*>();
  if(!proc){
    throw zs_error("to-macro-procedure: error: should be called with interpreted proc\n");
  }

  auto info = *proc->info();
  info.passing = Passing::quote;
  info.returning = Returning::code;

  return new IProcedure(proc->get(), 
                        info, proc->arg_list(),
                        proc->closure());
}

Lisp_ptr gensym(){
  static const string gensym_symname = {"(gensym)"};
  ZsArgs args{0};
  return {new Symbol(&gensym_symname)};
}

Lisp_ptr exit_func(){
  ZsArgs args{0};
  // cerr << "exiting..\n";
  vm.stack.clear();
  vm.code.clear();
  return {};
}
