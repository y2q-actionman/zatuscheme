#include <cassert>

#include "vm.hh"
#include "env.hh"

VM vm;

VM::VM() : code(), stack(),
               frame_(new Env(nullptr)),
               frame_history_(),
               symtable_(new SymTable())
{
  frame_->add_ref();
}

VM::~VM(){
  if(frame_->release() <= 0){
    delete frame_;
  }
  for(auto e : frame_history_){
    if(e->release() <= 0){
      delete e;
    }
  }
}

void VM::enter_frame(Env* e){
  frame_history_.push_back(frame_);
  frame_ = e;
  frame_->add_ref();
}  

void VM::leave_frame(){
  if(frame_->release() <= 0){
    delete frame_;
  }
  frame_ = frame_history_.back();
  frame_history_.pop_back();
}  
