#include "builtin_procedure.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "eval.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

Lisp_ptr type_check_procedure(){
  ZsArgs args{1};
  return Lisp_ptr{(args[0].tag() == Ptr_tag::i_procedure) || (args[0].tag() == Ptr_tag::n_procedure)};
}

Lisp_ptr proc_values(){
  vm.return_value.clear();

  stack_to_vector(vm.stack, vm.return_value);

  if(vm.return_value.empty()){
    vm.return_value.resize(1);
  }

  return vm_op_nop;
}
