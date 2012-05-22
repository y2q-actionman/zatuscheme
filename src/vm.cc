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
    if(auto p = env_find(*e, s)){
      return p;
    }
  }

  return {};
}

Lisp_ptr VM_t::local_set(Symbol* s, Lisp_ptr p){
  for(auto e = frames_.rbegin(); e != frames_.rend(); ++e){
    if(env_find(*e, s)){
      return env_set(*e, s, p);
    }
  }

  return env_set(frames_.back(), s, p);
}

Lisp_ptr VM_t::global_set(Symbol* s, Lisp_ptr p){
  return env_set(frames_.front(), s, p);
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

inline
Lisp_ptr VM_t::env_find(const Env& e, Symbol* s){
  auto i = e.find(s);
  return (i != e.end()) ? i->second : Lisp_ptr{};
}

inline
Lisp_ptr VM_t::env_set(Env& e, Symbol* s, Lisp_ptr p){
  Lisp_ptr ret = {};
  auto i = e.find(s);

  if(i != e.end()){
    ret = i->second;
    e.erase(i);
  }
  
  e.insert({s, p});
  return ret;
}
