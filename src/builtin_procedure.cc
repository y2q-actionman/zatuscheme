#include <algorithm>
#include <iterator>

#include "builtin_procedure.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "eval.hh"

using namespace std;
using namespace Procedure;

namespace {

void type_check_procedure(){
  auto arg = pick_args_1();
  vm.return_value[0] = Lisp_ptr{(arg.tag() == Ptr_tag::i_procedure)
                             || (arg.tag() == Ptr_tag::n_procedure)};
}

void proc_values(){
  vm.return_value.resize(0);

  while(1){
    if(vm.stack.back().tag() == Ptr_tag::vm_op){
      vm.stack.pop_back();
      break;
    }

    vm.return_value.push_back(vm.stack.back());
    vm.stack.pop_back();
  }

  if(vm.return_value.empty()){
    vm.return_value.resize(1);
  }
}

} // namespace

const BuiltinFunc
builtin_procedure[] = {
  {"procedure?", {
      type_check_procedure,
      {Calling::function, 1}}},
  {"apply", {
      apply_func,
      {Calling::function, 1, Variadic::t}}},
  {"force", {
      func_force,
      {Calling::function, 1}}},
  {"values", {
      proc_values,
      {Calling::function, 0, Variadic::t}}},
  {"call-with-values", {
      call_with_values,
      {Calling::function, 2}}},
};

const size_t builtin_procedure_size = sizeof(builtin_procedure) / sizeof(builtin_procedure[0]);
