#ifndef VM_I_HH
#define VM_I_HH

#ifndef VM_HH
#error "Please include via parent file"
#endif

inline
ZsArgs::ZsArgs()
  : size_(vm.stack.back().get<int>()),
    stack_iter_s_(vm.stack.end() - (size_ + 1)){}

inline
ZsArgs::ZsArgs(ZsArgs&& other)
  : size_(other.size_),
    stack_iter_s_(move(other.stack_iter_s_)){
  other.invalidate();
}

inline
ZsArgs::~ZsArgs(){
  cleanup();
}

inline
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

#endif // VM_I_HH
