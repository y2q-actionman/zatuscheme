#include "stack.hh"

Lisp_ptr Stack::find(Symbol* s) const {
  for(auto i = stack_.rbegin(); i != stack_.rend(); ++i){
    if(i->first == s) return i->second;
  }

  return {};
}

void Stack::push(Symbol* s, Lisp_ptr p){
  stack_.push_back({s, p});
}

void Stack::pop(int i){
  auto it = stack_.end() - i;
  stack_.erase(it, stack_.end());
}
