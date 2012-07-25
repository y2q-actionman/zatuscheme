#include "cons.hh"

static_assert(sizeof(Cons) == sizeof(Lisp_ptr) * 2,
              "cons cell sizing failed");

// for non-constexpr context
constexpr Lisp_ptr Cons::NIL;

static_assert(Cons::NIL.get<void*>() == nullptr,
              "expressiong NIL failed.");

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

Lisp_ptr make_cons_list(std::initializer_list<Lisp_ptr> lis){
  if(lis.size() <= 0) return Cons::NIL;

  auto i = begin(lis);
  const auto e = end(lis);

  Cons* head = new Cons;
  Cons* c = head;

  while(1){
    c->rplaca(*i);

    ++i;
    if(i == e) break;

    Cons* newc = new Cons;
    c->rplacd(Lisp_ptr(newc));
    c = newc;
  }

  c->rplacd(Cons::NIL);
  return Lisp_ptr(head);
}
