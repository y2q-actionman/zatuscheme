#ifndef EQUALITY_HH
#define EQUALITY_HH

#include <cstddef>
#include "lisp_ptr.hh"

namespace zs {

bool eq_internal(Lisp_ptr, Lisp_ptr);
bool eqv_internal(Lisp_ptr, Lisp_ptr);
bool equal_internal(Lisp_ptr, Lisp_ptr);

size_t eq_hash(Lisp_ptr);

// function object
struct EqObj;
struct EqHashObj;

} // namespace zs

#include "equality.i.hh"

#endif // EQUALITY_HH
