#ifndef BUILTIN_EQUAL_HH
#define BUILTIN_EQUAL_HH

#include "decl.hh"

bool eq_internal(Lisp_ptr a, Lisp_ptr b);
bool eqv_internal(Lisp_ptr a, Lisp_ptr b);
bool equal_internal(Lisp_ptr a, Lisp_ptr b);

Lisp_ptr eq();
Lisp_ptr eqv();
Lisp_ptr equal();

#endif // BUILTIN_EQUAL_HH
