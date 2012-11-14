#include "builtin_util.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include <iterator>

Lisp_ptr pick_args_1(){
  auto tmp = pick_args<1>();
  return tmp[0];
}

ArgAccessor::ArgAccessor(VM& v)
  : the_vm_(v),
    stack_iter_s_(),
    stack_iter_e_(v.stack.end()){
  auto argcnt = v.stack.back().get<int>();
  stack_iter_s_  = stack_iter_e_ - (argcnt + 1);
}

ArgAccessor::ArgAccessor(int request_argc, VM& v)
  : the_vm_(v),
    stack_iter_s_(),
    stack_iter_e_(v.stack.end()){
  // TODO: use delegating constructor
  auto argcnt = v.stack.back().get<int>();
  stack_iter_s_  = stack_iter_e_ - (argcnt + 1);

  if(argcnt != request_argc){
    // throw exception.
  }
}

ArgAccessor::~ArgAccessor(){
  the_vm_.stack.erase(stack_iter_s_, stack_iter_e_);
}

void builtin_type_check_failed(const char* func_name, Ptr_tag tag, Lisp_ptr p){
  fprintf(zs::err, "native func: %s: arg is not %s! (%s)\n",
          func_name, stringify(tag), stringify(p.tag()));
  vm.return_value[0] = {};
}

void builtin_variadic_argcount_failed(const char* name, int argc){
  fprintf(zs::err, "native func: %s: %d or more args.\n", name, argc+1);
  vm.return_value[0] = {};
}

