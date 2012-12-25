#ifndef BUILTIN_VECTOR_HH
#define BUILTIN_VECTOR_HH

#include "decl.hh"

Lisp_ptr vector_make();
Lisp_ptr vector_vector();
Lisp_ptr vector_length();
Lisp_ptr vector_ref();
Lisp_ptr vector_set();
Lisp_ptr vector_to_list();
Lisp_ptr vector_from_list();
Lisp_ptr vector_fill();

#endif // BUILTIN_VECTOR_HH
