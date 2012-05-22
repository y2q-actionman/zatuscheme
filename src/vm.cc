#include "vm.hh"

VM_t VM;

Lisp_ptr VM_t::find(Symbol* s) const{
  if(auto v1 = stack.find(s)){
    return v1;
  }else if(auto v2 = env.find(s)){
    return v2;
  }else{
    return {};
  }
}

Lisp_ptr VM_t::local_set(Symbol* s, Lisp_ptr p){
  if(stack.find(s)){
    return stack.set(s, p);
  }else if(env.find(s)){
    return env.set(s, p);
  }else{
    return {};
  }
}

Lisp_ptr VM_t::global_set(Symbol* s, Lisp_ptr p){
  return env.set(s, p);
}

void VM_t::push(Symbol* s, Lisp_ptr p){
  stack.push(s, p);
}

void VM_t::pop(int i){
  stack.pop(i);
}

Lisp_ptr VM_t::at(int i) const{
  return stack.at(i);
}

int VM_t::size() const{
  return stack.size();
}

