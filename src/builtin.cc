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
#include "builtin_numeric.hh"
#include "builtin_symbol.hh"
#include "builtin_syntax.hh"

using namespace std;
using namespace Procedure;

namespace {

void type_check_procedure(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{(arg.tag() == Ptr_tag::i_procedure)
                             || (arg.tag() == Ptr_tag::n_procedure)};
}
  

bool eq_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;

  if(a.tag() == Ptr_tag::boolean){
    return a.get<bool>() == b.get<bool>();
  }else if(a.tag() == Ptr_tag::character){
     // this can be moved into eqv? in R5RS, but char is contained in Lisp_ptr.
    return a.get<char>() == b.get<char>();
  }else{
    return a.get<void*>() == b.get<void*>();
  }
}

bool eqv_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() == Ptr_tag::number && b.tag() == Ptr_tag::number){
    return eqv(*a.get<Number*>(), *b.get<Number*>());
  }else{
    return eq_internal(a, b);
  }
}

void eq(){
  auto args = pick_args<2>();
  VM.return_value = Lisp_ptr{eq_internal(args[0], args[1])};
}

void eqv(){
  auto args = pick_args<2>();
  VM.return_value = Lisp_ptr{eqv_internal(args[0], args[1])};
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
  {"vector", {
      procedure_vector, 
      {Calling::function, 1, Variadic::t}}},
  {"vector?", {
      type_check_pred<Ptr_tag::vector>,
      {Calling::function, 1}}},
  {"procedure?", {
      type_check_procedure,
      {Calling::function, 1}}},
  {"string?", {
      type_check_pred<Ptr_tag::string>,
      {Calling::function, 1}}},
  {"port?", {
      type_check_pred<Ptr_tag::port>,
      {Calling::function, 1}}},
  {"eqv?", {
      eqv,
      {Calling::function, 2}}},
  {"eq?", {
      eq,
      {Calling::function, 2}}},

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
  install_builtin_internal(builtin_misc, builtin_misc_size);
  install_builtin_internal(builtin_boolean, builtin_boolean_size);
  install_builtin_internal(builtin_char, builtin_char_size);
  install_builtin_internal(builtin_cons, builtin_cons_size);
  install_builtin_internal(builtin_numeric, builtin_numeric_size);
  install_builtin_internal(builtin_symbol, builtin_symbol_size);
  install_builtin_internal(builtin_syntax, builtin_syntax_size);
}
