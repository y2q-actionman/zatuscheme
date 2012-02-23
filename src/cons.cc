#include "cons.hh"

static_assert(sizeof(Cons) == sizeof(Lisp_ptr) * 2,
              "cons cell sizing failed");

static_assert(static_cast<bool>(Cons::NIL),
              "constructing NIL failed");

// for non-constexpr context
constexpr Lisp_ptr Cons::NIL;
