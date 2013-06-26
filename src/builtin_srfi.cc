#include <iostream>

#include "builtin_srfi.hh"
#include "zs_error.hh"
#include "procedure.hh"
#include "eval.hh"
#include "printer.hh"

using namespace std;

namespace builtin {

Lisp_ptr error(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(Ptr_tag::string, args[0]);
  }

  throw zs_error(*str);
}

Lisp_ptr with_exception_handler(ZsArgs args){
  Lisp_ptr handler = args[0];
  if(!is_procedure(handler.tag())){
    throw zs_error("arg is not procedure!", {handler});
  }

  Lisp_ptr thunk = args[1];
  if(!is_procedure(thunk.tag())){
    throw zs_error("arg is not procedure!", {thunk});
  }

  args.cleanup();

  vm.exception_handler.push_back(handler);
  vm.code.push_back(vm_op_unwind_guard);
  vm.stack.push_back(vm_op_unwind_guard);

  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.insert(vm.code.end(), {thunk, vm_op_proc_enter});
  return {};
}

Lisp_ptr raise(ZsArgs args){
  vm.code.push_back(vm_op_raise);
  return args[0];
}

} // namespace builtin
