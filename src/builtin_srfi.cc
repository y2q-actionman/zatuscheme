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

  if(get_procinfo(handler)->required_args != 1){
    throw zs_error("handler must take 1 arg");
  }

  Lisp_ptr thunk = args[1];
  if(!is_procedure(thunk.tag())){
    throw zs_error_arg1(nullptr, "arg is not procedure!", {thunk});
  }

  if(get_procinfo(thunk)->required_args != 0){
    throw zs_error("handler must take 0 arg");
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
  auto errobj = args[0];
  args.cleanup();

  while(!vm.code.empty()){
    auto p = vm.code.back();
    vm.code.pop_back();
    if(p.get<VMop>() == vm_op_unwind_guard) break;
  }
      
  while(!vm.stack.empty()){
    auto p = vm.stack.back();
    vm.stack.pop_back();
    if(p.get<VMop>() == vm_op_unwind_guard) break;
  }

  if(!vm.exception_handler.empty()){
    auto handler = vm.exception_handler.back();
    vm.exception_handler.pop_back();
    vm.stack.push_back(errobj);
    vm.stack.push_back({Ptr_tag::vm_argcount, 1});
    vm.code.insert(vm.code.end(), {handler, vm_op_proc_enter});
  }else{      
    cerr << "uncaught exception\n"
         << errobj << '\n'
         << "VM dump...\n"
         << vm << endl;
  }
  return {};
}

} // namespace builtin
