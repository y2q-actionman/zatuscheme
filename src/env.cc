#include <ostream>

#include "env.hh"
#include "printer.hh"
#include "zs_memory.hh"

using namespace std;

Env::Env(Env* e)
  : map_(), next_(e){
}

Env::~Env(){}

template<typename Fun>
Lisp_ptr Env::traverse(Lisp_ptr key, Fun f){
  for(auto e = this; e; e = e->next_){
    auto ei = e->map_.find(key);
    if(ei != e->map_.end()){
      return f(e, ei);
    }
  }
  return {};
}

bool Env::is_bound(Lisp_ptr s){
  return static_cast<bool>
    (traverse(s, [](Env*, Env::map_type::iterator){
        return Lisp_ptr{true};
      }));
}

Lisp_ptr Env::find(Lisp_ptr s){
  return traverse(s, [](Env*, Env::map_type::iterator ei){
      return ei->second;
    });
}

void Env::set(Lisp_ptr s, Lisp_ptr p){
  traverse(s, [&](Env* e, Env::map_type::iterator ei) -> Lisp_ptr{
      e->map_.erase(ei);
      e->map_.insert({s, p});
      return {};
    });
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
