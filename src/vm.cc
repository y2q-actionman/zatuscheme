#include <cassert>

#include "vm.hh"
#include "env.hh"

VM_t VM;

VM_t::VM_t() : symtable(), code(), stack(),
               frame(new Env(nullptr)),
               frame_history_(){
  frame->add_ref();
}

VM_t::~VM_t(){
  frame->release();
}

void VM_t::enter_frame(Env* e){
  frame_history_.push(frame);
  frame = e;
  frame->add_ref();
}  

void VM_t::leave_frame(){
  if(frame->release() <= 0){
    delete frame;
  }
  frame = frame_history_.top();
  frame_history_.pop();
}  
