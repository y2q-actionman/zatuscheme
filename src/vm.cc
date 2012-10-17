#include <cassert>

#include "vm.hh"
#include "env.hh"

VM vm;

namespace {

template<typename T>
void add_ref(T& frame_seq){
  for(auto e : frame_seq){
    add_ref(e);
  }
}

template<typename T>
void release(T& frame_seq){
  for(auto e : frame_seq){
    release(e);
  }
}

} // namespace


VM::VM() : code(), stack(),
           return_value(1, {}),
           frames_(),
           symtable_(new SymTable())
{
  enter_frame(new Env{nullptr});
}

VM::VM(const VM& other) : code(other.code), stack(other.stack),
                          return_value(other.return_value),
                          frames_(other.frames_),
                          symtable_(other.symtable_)
{
  add_ref(frames_);
}

VM::~VM(){
  release(frames_);
}


VM& VM::operator=(const VM& other){
  code = other.code;
  stack = other.stack;
  return_value = other.return_value;

  release(frames_);
  add_ref(other.frames_);
  frames_ = other.frames_;
  
  symtable_ = other.symtable_;

  return *this;
}


void VM::enter_frame(Env* e){
  frames_.push_back(e);
  add_ref(frame());
}  

void VM::leave_frame(){
  release(frame());
  frames_.pop_back();
}
