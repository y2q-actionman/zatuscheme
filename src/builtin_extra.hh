#ifndef BUILTIN_EXTRA_HH
#define BUILTIN_EXTRA_HH

#include "decl.hh"

Lisp_ptr traditional_transformer();
Lisp_ptr sc_macro_transformer();

Lisp_ptr gensym();
Lisp_ptr exit_func();

#endif // BUILTIN_EXTRA_HH
