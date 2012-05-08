#include "cons.hh"

static_assert(sizeof(Cons) == sizeof(Lisp_ptr) * 2,
              "cons cell sizing failed");

// for non-constexpr context
constexpr Lisp_ptr Cons::NIL;

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
