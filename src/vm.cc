#include <cassert>

#include "vm.hh"
#include "env.hh"

VM vm;

VM::VM() : code(), stack(),
               frame(new Env(nullptr)),
               frame_history_(),
               symtable_(new SymTable())
{
  frame->add_ref();
}

VM::~VM(){
  frame->release();
  for(auto e : frame_history_){
    if(e->release() <= 0){
      delete e;
    }
  }
}

void VM::enter_frame(Env* e){
  frame_history_.push_back(frame);
  frame = e;
  frame->add_ref();
}  

void VM::leave_frame(){
  if(frame->release() <= 0){
    delete frame;
  }
  frame = frame_history_.back();
  frame_history_.pop_back();
}  
