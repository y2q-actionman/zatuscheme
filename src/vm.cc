#include "vm.hh"
#include "cons.hh"

VM_t VM;

VM_t::VM_t() : frames_(new Cons(Lisp_ptr(new Env()), Cons::NIL)),
               args_() {
  args_.reserve(16); // tekitou!
}

void VM_t::enter_frame(Env* e){
  frames_ = Lisp_ptr(new Cons(Lisp_ptr(e), frames_));
}  

void VM_t::leave_frame(){
  frames_ = frames_.get<Cons*>()->cdr();
}  

Lisp_ptr VM_t::find(Symbol* s) const{
  Lisp_ptr ret;

  do_list(frames_,
          [&](Cons* c) -> bool {
            auto e = c->car().get<Env*>();
            auto ei = e->find(s);
            if(ei != e->end()){
              ret = ei->second;
              return false;
            }
            return true;
          },
          [](Lisp_ptr){});

  return ret;
}

void VM_t::set(Symbol* s, Lisp_ptr p){
  bool found = false;

  do_list(frames_,
          [&](Cons* c) -> bool {
            auto e = c->car().get<Env*>();
            auto ei = e->find(s);
            if(ei != e->end()){
              found = true;
              e->erase(ei);
              e->insert({s, p});
              return false;
            }
            return true;
          },
          [](Lisp_ptr){});

  if(!found){
    auto front = frames_.get<Cons*>()->car().get<Env*>();
    front->insert({s, p});
  }
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
