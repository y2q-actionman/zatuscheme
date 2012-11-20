#include "builtin_util.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include <iterator>

Lisp_ptr pick_args_1(){
  auto tmp = pick_args<1>();
  return tmp[0];
}

ArgAccessor::ArgAccessor()
  : stack_iter_s_(),
    stack_iter_e_(vm.stack.end()){
  auto argcnt = vm.stack.back().get<int>();
  stack_iter_s_  = stack_iter_e_ - (argcnt + 1);
}

ArgAccessor::ArgAccessor(int request_argc)
  : stack_iter_s_(),
    stack_iter_e_(vm.stack.end()){
  // TODO: use delegating constructor
  auto argcnt = vm.stack.back().get<int>();
  stack_iter_s_  = stack_iter_e_ - (argcnt + 1);

  if(argcnt != request_argc){
    // throw exception.
  }
}

ArgAccessor::~ArgAccessor(){
  vm.stack.erase(stack_iter_s_, stack_iter_e_);
}

zs_error builtin_type_check_failed(const char* func_name, Ptr_tag tag, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(tag), stringify(p.tag()));
}

zs_error builtin_variadic_argcount_failed(const char* name, int argc){
  return make_zs_error("native func: %s: %d or more args.\n", name, argc+1);
}

