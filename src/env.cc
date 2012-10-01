#include "env.hh"
#include <utility>

using namespace std;

Env::Env()
  : map_(), next_(nullptr), refcnt_(0){}

Env::Env(const Env& e)
  : map_(e.map_), next_(e.next_), refcnt_(0){
  if(next_) next_->add_ref();
}

Env::Env(Env&& e)
  : map_(move(e.map_)), next_(e.next_), refcnt_(0){
  e.next_ = nullptr;
}

Env::Env(Env* e)
  : map_(), next_(e), refcnt_(0){
  if(next_) next_->add_ref();
}

Env::~Env(){
  if(next_ && next_->release() <= 0){
    delete next_;
  }
}

Env& Env::operator=(const Env& e){
  map_ = e.map_;

  if(next_ && next_->release() <= 0){
    delete next_;
  }
  next_ = e.next_;
  if(next_) next_->add_ref();

  return *this;
}

Env& Env::operator=(Env&& e){
  map_ = move(e.map_);

  next_ = e.next_;
  e.next_ = nullptr;

  return *this;
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
  
int Env::add_ref(){
  return ++refcnt_;
}

int Env::release(){
  return --refcnt_;
}
