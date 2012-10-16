#include <cassert>

#include "vm.hh"
#include "env.hh"

VM vm;

VM::VM() : code(), stack(),
           frames_(),
           symtable_(new SymTable())
{
  enter_frame(new Env{nullptr});
}

VM::~VM(){
  for(auto e : frames_){
    if(e->release() <= 0){
      delete e;
    }
  }
}

void VM::enter_frame(Env* e){
  frames_.push_back(e);
  frame()->add_ref();
}  

void VM::leave_frame(){
  if(frame()->release() <= 0){
    delete frame();
  }
  frames_.pop_back();
}
