#include "cons_util.hh"
#include "lisp_ptr.hh"
#include "cons.hh"

#include <cassert>

void free_cons_list(Lisp_ptr p){
  // TODO: erase elements!

  do_list(p,
          [](Cons* c) -> bool {
            // erase c->car()
            delete c;
            return true;
          },
          [](Lisp_ptr dot_cdr){
            if(!nullp(dot_cdr)){
              // erase element
              ;
            }
          });
}

Lisp_ptr push_cons_list(Lisp_ptr p, Lisp_ptr q){
  return Lisp_ptr(new Cons(p, q));
}

void GrowList::push(Lisp_ptr p){
  assert(head && next);
  assert(*next == Cons::NIL);

  auto newc = new Cons(p, Cons::NIL);
  
  *next = {newc};
  next = &(newc->cdr_);
}
