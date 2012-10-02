#include <array>

#include "builtin.hh"
#include "util.hh"
#include "number.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "printer.hh"

using namespace std;
using namespace Procedure;

namespace {

template <Ptr_tag p>
void type_check_pred(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{arg.tag() == p};
}

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

  VM.return_value = new IProcedure(proc->get(), Calling::macro,
                                   proc->info(), proc->arg_head(),
                                   proc->closure());
}

constexpr BuiltinFunc
builtin_func[] = {
  // functions
  {"vector", {
      procedure_vector, 
      Calling::function, {1, true}}},
  {"vector?", {
      type_check_pred<Ptr_tag::vector>,
      Calling::function, {1, false}}},
  {"procedure?", {
      type_check_procedure,
      Calling::function, {1, false}}},
  {"string?", {
      type_check_pred<Ptr_tag::string>,
      Calling::function, {1, false}}},
  {"port?", {
      type_check_pred<Ptr_tag::port>,
      Calling::function, {1, false}}},
  {"eqv?", {
      eqv,
      Calling::function, {2, false}}},
  {"eq?", {
      eq,
      Calling::function, {2, false}}},

  {"eval", {
      eval_func,
      Calling::function, {2, false}}},
  {"to-macro-procedure", {
      to_macro_procedure,
      Calling::function, {1, false}}}
};

} //namespace

void install_builtin(){
  for(auto& e : builtin_func){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
