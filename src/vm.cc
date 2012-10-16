#include <cassert>

#include "vm.hh"
#include "env.hh"

VM vm;

VM::VM() : code(), stack(),
           return_value(1, {}),
           frames_(),
           symtable_(new SymTable())
{
  enter_frame(new Env{nullptr});
}

VM::~VM(){
  for(auto e : frames_){
    release(e);
  }
}

void VM::enter_frame(Env* e){
  frames_.push_back(e);
  add_ref(frame());
}  

void VM::leave_frame(){
  release(frame());
  frames_.pop_back();
}
