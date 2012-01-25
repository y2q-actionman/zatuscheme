#include "cons.hh"

static_assert(sizeof(Cons) == sizeof(Lisp_ptr) * 2,
              "cons cell sizing failed");

const Lisp_ptr Cons::NIL{static_cast<Cons*>(nullptr)};
