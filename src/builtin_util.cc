#include <exception>

#include "builtin_util.hh"

Lisp_ptr pick_args_1(){
  auto arg1 = pick_args<1>();
  return arg1[0];
}

zs_error builtin_type_check_failed(const char* func_name, Ptr_tag tag, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(tag), stringify(p.tag()));
}

zs_error builtin_argcount_failed(const char* name, int required, int max, int passed){
  throw make_zs_error("eval error: %s: number of passed args is mismatched!!"
                      " (acceptable %d-%d args, passed %d)\n",
                      name, required, max, passed);
}

// class ZsArgs
ZsArgs::ZsArgs()
  : stack_iter_s_(),
    stack_iter_e_(vm.stack.end()){
  auto argcnt = vm.stack.back().get<int>();
  stack_iter_s_  = stack_iter_e_ - (argcnt + 1);
}

ZsArgs::ZsArgs(int i)
  : stack_iter_s_(),
    stack_iter_e_(vm.stack.end()){
  auto argcnt = vm.stack.back().get<int>();
  stack_iter_s_  = stack_iter_e_ - (argcnt + 1);

  if(i != argcnt)
    throw builtin_argcount_failed("(unknown func)", i, i, argcnt);
}
/*
ZsArgs::ZsArgs(ZsArgs&& other)
  : stack_iter_s_(move(other.stack_iter_s_)),
    stack_iter_e_(move(other.stack_iter_e_)){
  other.invalidate();
}
*/
ZsArgs::~ZsArgs(){
  // if(!(stack_iter_s_ < stack_iter_e_)) return;

  if(!std::uncaught_exception()){
    vm.stack.erase(stack_iter_s_, stack_iter_e_);
    invalidate();
  }
}
/*
ZsArgs& ZsArgs::operator=(ZsArgs&& other){
  stack_iter_s_ = move(other.stack_iter_s_);
  stack_iter_e_ = move(other.stack_iter_e_);
  other.invalidate();
  return *this;
}
*/
void ZsArgs::invalidate(){
  // stack_iter_s_ = stack_iter_e_ = decltype(vm.stack.end()){};
}
