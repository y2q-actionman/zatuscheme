#ifndef BUILTIN_EQUAL_HH
#define BUILTIN_EQUAL_HH

#include "decl.hh"

// eq? is same as 'operator==(Lisp_ptr, Lisp_ptr)'.
bool eqv_internal(Lisp_ptr a, Lisp_ptr b);
bool equal_internal(Lisp_ptr a, Lisp_ptr b);

Lisp_ptr eq();
Lisp_ptr eqv();
Lisp_ptr equal();

#endif // BUILTIN_EQUAL_HH
