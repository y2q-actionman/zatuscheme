#include "env.hh"

Lisp_ptr Env::find(Symbol* s) const{
  auto i = map_.find(s);
  return (i != map_.end()) ? i->second : Lisp_ptr{};
}

Lisp_ptr Env::set(Symbol* s, Lisp_ptr p){
  Lisp_ptr ret = {};
  auto i = map_.find(s);

  if(i != map_.end()){
    ret = i->second;
    map_.erase(i);
  }
  
  map_.insert({s, p});
  return ret;
}
