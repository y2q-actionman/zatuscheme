#include <cstdio>

#include "stack.hh"
#include "symbol.hh"

Lisp_ptr Stack::find(Symbol* s) const {
  for(auto i = stack_.rbegin(); i != stack_.rend(); ++i){
    if(i->first == s) return i->second;
  }

  return {};
}

Lisp_ptr Stack::set(Symbol* s, Lisp_ptr p){
  for(auto i = stack_.rbegin(); i != stack_.rend(); ++i){
    if(i->first == s){
      auto ret = i->second;
      i->second = p;
      return ret;
    }
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

Lisp_ptr Stack::at(int i) const{
  if(i >= 0){
    return stack_.at(i).second;
  }else{
    return stack_.at(stack_.size()+i).second;
  }
}

void describe(FILE* f, const Stack& s){
  for(auto i = s.stack_.rbegin(); i != s.stack_.rend(); ++i){
    fprintf(f, "[stack] %s = ",
            (i->first) ? i->first->name().c_str() : "(unnamed)");
    describe(f, i->second);
    fputc('\n', f);
  }
}
