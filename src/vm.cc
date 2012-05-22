#include "vm.hh"

VM_t VM;

VM_t::VM_t() {
  frames_.push_back({});
}

void VM_t::enter_frame(){
  frames_.push_back({});
}  

void VM_t::leave_frame(){
  frames_.pop_back();
}  

int VM_t::frame_depth() const{
  return frames_.size();
}

Lisp_ptr VM_t::find(Symbol* s) const{
  for(auto e = frames_.rbegin(); e != frames_.rend(); ++e){
    if(auto p = e->find(s)){
      return p;
    }
  }

  return {};
}

Lisp_ptr VM_t::local_set(Symbol* s, Lisp_ptr p){
  for(auto e = frames_.rbegin(); e != frames_.rend(); ++e){
    if(e->find(s)){
      return e->set(s, p);
    }
  }

  return frames_.back().set(s, p);
}

Lisp_ptr VM_t::global_set(Symbol* s, Lisp_ptr p){
  return frames_.front().set(s, p);
}

void VM_t::arg_push(Lisp_ptr p){
  args_.push_back(p);
}

Lisp_ptr VM_t::arg_get(int i) const{
  return args_.at(i);
}

void VM_t::arg_clear(){
  args_.clear();
}
