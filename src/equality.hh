#ifndef EQUALITY_HH
#define EQUALITY_HH

#include <functional>
#include <cstddef>
#include "lisp_ptr.hh"

bool eq_internal(Lisp_ptr, Lisp_ptr);
bool eqv_internal(Lisp_ptr, Lisp_ptr);
bool equal_internal(Lisp_ptr, Lisp_ptr);

size_t eq_hash(const Lisp_ptr&);

// function object
struct eq_obj;
struct eq_hash_obj;

#include "equality.i.hh"

#endif // EQUALITY_HH
