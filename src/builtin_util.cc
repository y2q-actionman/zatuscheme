#include <exception>

#include "builtin_util.hh"

// Lisp_ptr pick_args_1(){
//   auto arg1 = pick_args<1>();
//   return arg1[0];
// }

// class ZsArgs
ZsArgs::ZsArgs()
  : size_(vm.stack.back().get<int>()),
    stack_iter_s_(vm.stack.end() - (size_ + 1)){}

ZsArgs::ZsArgs(int i)
  : size_(vm.stack.back().get<int>()),
    stack_iter_s_(vm.stack.end() - (size_ + 1)){
  if(i != size_)
    throw builtin_argcount_failed("(unknown func)", i, i, size_);
}

ZsArgs::ZsArgs(ZsArgs&& other)
  : size_(other.size_),
    stack_iter_s_(move(other.stack_iter_s_)){
  other.invalidate();
}

ZsArgs::~ZsArgs(){
  if(!valid()) return;

  if(!std::uncaught_exception()){
    vm.stack.erase(this->begin(), this->end() + 1);
    invalidate();
  }
}

ZsArgs& ZsArgs::operator=(ZsArgs&& other){
  size_ = other.size_;
  stack_iter_s_ = move(other.stack_iter_s_);
  other.invalidate();
  return *this;
}

inline
void ZsArgs::invalidate(){
  size_ = -1;
}

inline
bool ZsArgs::valid() const{
  return (size_ >= 0);
}
