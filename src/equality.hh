#ifndef EQUALITY_HH
#define EQUALITY_HH

#include <cstddef>
#include "lisp_ptr.hh"

namespace zs {

bool eq(Lisp_ptr, Lisp_ptr);
bool eqv(Lisp_ptr, Lisp_ptr);
bool equal(Lisp_ptr, Lisp_ptr);

size_t eq_hash(Lisp_ptr);

// function object
struct EqObj;
struct EqHashObj;

} // namespace zs

#include "equality.i.hh"

#endif // EQUALITY_HH
