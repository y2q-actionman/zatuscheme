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
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  throw zs_error_arg1(nullptr, *str);
}

Lisp_ptr with_exception_handler(ZsArgs args){
  Lisp_ptr handler = args[0];
  if(!is_procedure(handler.tag())){
    throw zs_error_arg1(nullptr, "arg is not procedure!", {handler});
  }

  auto h_info = get_procinfo(handler);
  if(h_info->required_args != 1){
    throw builtin_argcount_failed(nullptr, h_info->required_args, h_info->max_args, 1);
  }

  Lisp_ptr thunk = args[1];
  if(!is_procedure(thunk.tag())){
    throw zs_error_arg1(nullptr, "arg is not procedure!", {thunk});
  }

  auto t_info = get_procinfo(thunk);
  if(t_info->required_args != 0){
    throw builtin_argcount_failed(nullptr, t_info->required_args, t_info->max_args, 0);
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
