#include <ostream>

#include "env.hh"
#include "printer.hh"
#include "zs_memory.hh"

using namespace std;

Env::Env(Env* e)
  : map_(), next_(e){
}

Env::~Env(){}

pair<Lisp_ptr, bool> Env::find(Lisp_ptr s){
  for(auto e = this; e; e = e->next_){
    auto ei = e->map_.find(s);
    if(ei != e->map_.end()){
      return {ei->second, true};
    }
  }
  return {{}, false};
}

void Env::set(Lisp_ptr s, Lisp_ptr p){
  for(auto e = this; e; e = e->next_){
    auto ei = e->map_.find(s);
    if(ei != e->map_.end()){
      e->map_.erase(ei);
      e->map_.insert({s, p});
      return;
    }
  }
}

void Env::local_set(Lisp_ptr s, Lisp_ptr p){
  this->map_[s] = p;
}

Env* Env::push(){
  return zs_new<Env>(this);
}

Env* Env::fork() const{
  auto ret = zs_new<Env>(nullptr);

  for(auto e = this; e; e = e->next_){
    ret->map_.insert(begin(e->map_), end(e->map_));
  }

  return ret;
}

std::ostream& operator<<(std::ostream& f, const Env& env){
  f << "Env " << reinterpret_cast<const void*>(&env)
    << " (next=" << reinterpret_cast<void*>(env.next_) << ")\n";
  for(auto e : env.map_){
    f << '\t' << e.first << "\t = " << e.second << '\n';
  }
  return f;
}
