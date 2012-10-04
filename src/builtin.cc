#include <array>

#include "builtin.hh"
#include "util.hh"
#include "number.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "vm.hh"

#include "builtin_boolean.hh"
#include "builtin_char.hh"
#include "builtin_cons.hh"
#include "builtin_equal.hh"
#include "builtin_numeric.hh"
#include "builtin_port.hh"
#include "builtin_string.hh"
#include "builtin_symbol.hh"
#include "builtin_syntax.hh"
#include "builtin_vector.hh"

using namespace std;
using namespace Procedure;

namespace {

void type_check_procedure(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{(arg.tag() == Ptr_tag::i_procedure)
                             || (arg.tag() == Ptr_tag::n_procedure)};
}
  

void eval_func(){
  auto args = pick_args<2>();
  
  // TODO: uses arg2 as Env struct.
  VM.code.push(args[0]);
}

void to_macro_procedure(){
  auto arg1 = pick_args_1();

  if(arg1.tag() != Ptr_tag::i_procedure){
    fprintf(zs::err, "to-macro-procedure: error: should be called with interpreted proc\n");
    VM.return_value = {};
    return;
  }

  auto proc = arg1.get<IProcedure*>();
  auto info = *proc->info();
  info.calling = Calling::macro;

  VM.return_value = new IProcedure(proc->get(), 
                                   info, proc->arg_head(),
                                   proc->closure());
}

} //namespace

const BuiltinFunc
builtin_misc[] = {
  {"procedure?", {
      type_check_procedure,
      {Calling::function, 1}}},

  {"eval", {
      eval_func,
      {Calling::function, 2}}},
  {"to-macro-procedure", {
      to_macro_procedure,
      {Calling::function, 1}}}
};

const size_t builtin_misc_size = sizeof(builtin_misc) / sizeof(builtin_misc[0]);

static void install_builtin_internal(const BuiltinFunc bf[], size_t s){
  for(size_t i = 0; i < s; ++i){
    VM.set(intern(VM.symtable, bf[i].name), {&bf[i].func});
  }
}

void install_builtin(){
  install_builtin_internal(builtin_syntax, builtin_syntax_size);

  install_builtin_internal(builtin_misc, builtin_misc_size);
  install_builtin_internal(builtin_boolean, builtin_boolean_size);
  install_builtin_internal(builtin_char, builtin_char_size);
  install_builtin_internal(builtin_cons, builtin_cons_size);
  install_builtin_internal(builtin_equal, builtin_equal_size);
  install_builtin_internal(builtin_numeric, builtin_numeric_size);
  install_builtin_internal(builtin_string, builtin_string_size);
  install_builtin_internal(builtin_symbol, builtin_symbol_size);
  install_builtin_internal(builtin_vector, builtin_vector_size);

  install_builtin_port_value();
  install_builtin_internal(builtin_port, builtin_port_size);
}
