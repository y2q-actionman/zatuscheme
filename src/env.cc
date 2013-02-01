#include <utility>
#include <ostream>

#include "env.hh"
#include "util.hh"
#include "symbol.hh"
#include "printer.hh"

using namespace std;

Env::Env(Env* e)
  : map_(), next_(e), foreign_binds_(){
}

Env::~Env(){
}

Lisp_ptr Env::find(Lisp_ptr s){
  for(Env* e = this; e; e = e->next_){
    auto ei = e->map_.find(s);
    if(ei != e->map_.end()){
      return ei->second;
    }
  }

  return {};
}

Lisp_ptr Env::set(Symbol* s, Lisp_ptr p){
  for(Env* e = this; e; e = e->next_){
    auto ei = e->map_.find(s);
    if(ei != e->map_.end()){
      auto old = ei->second;
      e->map_.erase(ei);
      e->map_.insert({s, p});
      return old;
    }
  }

  return {};
}

void Env::local_set(Lisp_ptr s, Lisp_ptr p){
  auto it = map_.find(s);
  if(it != map_.end()) map_.erase(it);

  map_.insert({s, p});
}

Env* Env::push(){
  return new Env{this};
}

Env* Env::fork() const{
  auto ret = new Env(nullptr);

  for(auto e = this; e; e = e->next_){
    ret->map_.insert(begin(e->map_), end(e->map_));
  }

  return ret;
}

std::ostream& operator<<(std::ostream& f, const Env& env){
  f << "Env " << c_cast<void*>(&env)
    << " (next=" << c_cast<void*>(env.next_) << ")\n";
  for(auto e : env.map_){
    f << '\t' << e.first << "\t = " << e.second << '\n';
  }
  return f;
}
