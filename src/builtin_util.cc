#include <exception>

#include "builtin_util.hh"
#include "zs_error.hh"

// class ZsArgs
// - invalidate() :: marks as unusable.
// - cleanup() :: destroys vm's arguments really.

ZsArgs::ZsArgs()
  : size_(vm.stack.back().get<int>()),
    stack_iter_s_(vm.stack.end() - (size_ + 1)){}

ZsArgs::ZsArgs(ZsArgs&& other)
  : size_(other.size_),
    stack_iter_s_(move(other.stack_iter_s_)){
  other.invalidate();
}

ZsArgs::~ZsArgs(){
  cleanup();
}

ZsArgs& ZsArgs::operator=(ZsArgs&& other){
  size_ = other.size_;
  stack_iter_s_ = move(other.stack_iter_s_);
  other.invalidate();
  return *this;
}

void ZsArgs::cleanup(){
  if(size_ < 0) return;

  if(!std::uncaught_exception()){
    vm.stack.erase(this->begin(), this->end() + 1);
    invalidate();
  }
}

inline
void ZsArgs::invalidate(){
  size_ = -1;
}

inline
bool ZsArgs::valid() const{
  return (size_ >= 0);
}
