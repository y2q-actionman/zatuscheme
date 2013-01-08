#include <utility>
#include <ostream>

#include "env.hh"
#include "util.hh"
#include "symbol.hh"
#include "printer.hh"

using namespace std;

Env::Env(Env* e)
  : map_(), next_(e){
}

Env::~Env(){
}

Lisp_ptr Env::traverse(Symbol* s, Lisp_ptr p){
  Lisp_ptr old = {};

  for(Env* e = this; e; e = e->next_){
    auto ei = e->map_.find(s);
    if(ei != e->map_.end()){
      old = ei->second;
      if(p){
        e->map_.erase(ei);
        e->map_.insert({s, p});
      }
      break;
    }
  }

  return old;
}

void Env::local_set(Symbol* s, Lisp_ptr p){
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

// void Env::clear(){
//   map_.clear();
// }

std::ostream& operator<<(std::ostream& f, const Env& env){
  f << "Env " << c_cast<void*>(&env)
    << " (next=" << c_cast<void*>(env.next_) << ")\n";
  for(auto e : env.map_){
    f << '\t' << e.first->name() << "\t = ";
    print(f, e.second);
    f << '\n';
  }
  return f;
}
