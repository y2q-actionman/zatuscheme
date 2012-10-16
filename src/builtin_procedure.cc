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
  unsigned i = 0;

  while(1){
    if(vm.stack.back().tag() == Ptr_tag::vm_op){
      vm.stack.pop_back();
      break;
    }

    vm.return_value[i] = vm.stack.back();
    vm.stack.pop_back();
    ++i;

    if(i >= VM::return_value_max){
      fprintf(zs::err, "eval warning: values: overed max values (%ud)\n",
              VM::return_value_max);
      clean_args();
      return;
    }
  }

  for(; i < VM::return_value_max; ++i){
    vm.return_value[i] = {};
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
