#include "builtin_extra.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

void to_macro_procedure(){
  auto arg1 = pick_args_1();

  if(arg1.tag() != Ptr_tag::i_procedure){
    throw zs_error("to-macro-procedure: error: should be called with interpreted proc\n");
  }

  auto proc = arg1.get<IProcedure*>();
  auto info = *proc->info();
  info.calling = Calling::macro;

  vm.return_value[0] = new IProcedure(proc->get(), 
                                   info, proc->arg_list(),
                                   proc->closure());
}

void gensym(){
  static const string gensym_symname = {"(gensym)"};
  pick_args<0>();
  vm.return_value[0] = {new Symbol(&gensym_symname)};
}

void exit_func(){
  pick_args<0>();
  // cerr << "exiting..\n";
  vm.return_value[0] = {};
  vm.stack.clear();
  vm.code.clear();
}

} // namespace

const BuiltinFunc
builtin_extra[] = {
  {"to-macro-procedure", {
      to_macro_procedure,
      {Calling::function, 1}}},
  {"gensym", {
      gensym,
      {Calling::function, 0}}},
  {"exit", {
      exit_func,
      {Calling::function, 0}}}
};

const size_t builtin_extra_size = sizeof(builtin_extra) / sizeof(builtin_extra[0]);


const char* builtin_extra_load[] = {
  "(define (read-eval-print-loop)"
  "  (let loop ()"
  "    (display \">> \")"
  "    (display (eval (read) (interaction-environment)))"
  "    (newline)"
  "    (loop)))",
};

const size_t builtin_extra_load_size
= sizeof(builtin_extra_load) / sizeof(builtin_extra_load[0]);
