#include "vm.hh"

VM_t VM;

VM_t::VM_t() : frame_(new Cons(Lisp_ptr(new Env()), Cons::NIL)),
               frame_history_(),
               args_() {
  args_.reserve(16); // tekitou!
}

Lisp_ptr VM_t::find(Symbol* s) const{
  Lisp_ptr ret;

  do_list(frame_,
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

  do_list(frame_,
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
    auto front = frame_.get<Cons*>()->car().get<Env*>();
    front->insert({s, p});
  }
}

Lisp_ptr push_frame(Lisp_ptr l){
  return Lisp_ptr(new Cons(Lisp_ptr(new Env), l));
}
