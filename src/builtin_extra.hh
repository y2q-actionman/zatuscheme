#ifndef BUILTIN_EXTRA_HH
#define BUILTIN_EXTRA_HH

#include "decl.hh"

Lisp_ptr to_macro_procedure();
Lisp_ptr gensym();
Lisp_ptr exit_func();

extern const char* builtin_extra_load[];
extern const size_t builtin_extra_load_size;

#endif // BUILTIN_EXTRA_HH
