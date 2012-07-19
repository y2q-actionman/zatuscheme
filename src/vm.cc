#include "vm.hh"

VM_t VM;

VM_t::VM_t() : symtable(), code(), stack(),
               frame(new Cons(Lisp_ptr(new Env()), Cons::NIL)),
               frame_history_(){}

Lisp_ptr VM_t::traverse(Symbol* s, Lisp_ptr p){
  Lisp_ptr old = {};

  do_list(frame,
          [&](Cons* c) -> bool {
            auto e = c->car().get<Env*>();
            auto ei = e->find(s);
            if(ei != e->end()){
              old = ei->second;
              if(p){
                e->erase(ei);
                e->insert({s, p});
              }
              return false;
            }
            return true;
          },
          [](Lisp_ptr){});

  return old;
}

void VM_t::local_set(Symbol* s, Lisp_ptr p){
  auto front = frame.get<Cons*>()->car().get<Env*>();

  auto it = front->find(s);
  if(it != front->end()) front->erase(it);

  front->insert({s, p});
}

Lisp_ptr push_frame(Lisp_ptr l){
  return Lisp_ptr(new Cons(Lisp_ptr(new Env), l));
}
