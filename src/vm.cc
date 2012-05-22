#include "vm.hh"

VM_t VM;

VM_t::VM_t() : frames_(1), args_() {
  args_.reserve(16); // tekitou!
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
    auto ei = e->find(s);
    if(ei != e->end())
      return ei->second;
  }

  return {};
}

Lisp_ptr VM_t::local_set(Symbol* s, Lisp_ptr p){
  for(auto e = frames_.rbegin(); e != frames_.rend(); ++e){
    auto ei = e->find(s);
    if(ei != e->end()){
      auto ret = ei->second;
      e->erase(ei);
      e->insert({s, p});
      return ret;
    }
  }

  frames_.back().insert({s, p});
  return {};
}

Lisp_ptr VM_t::global_set(Symbol* s, Lisp_ptr p){
  auto& ev = frames_.front();

  auto i = ev.find(s);
  if(i != ev.end()){
    auto ret = i->second;
    ev.erase(i);
    return ret;
  }
  
  ev.insert({s, p});
  return {};
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
