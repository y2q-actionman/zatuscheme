#include <cassert>

#include "vm.hh"
#include "env.hh"

VM_t VM;

VM_t::VM_t() : symtable(), code(), stack(),
               frame(new Env()),
               frame_history_(){
  frame->add_ref();
}

VM_t::~VM_t(){
  frame->release();
}

Lisp_ptr VM_t::traverse(Symbol* s, Lisp_ptr p){
  return frame->traverse(s, p);
}

void VM_t::local_set(Symbol* s, Lisp_ptr p){
  frame->local_set(s, p);
}
