#ifndef BUILTIN_EXTRA_HH
#define BUILTIN_EXTRA_HH

#include "decl.hh"

Lisp_ptr traditional_transformer();
Lisp_ptr sc_macro_transformer();

Lisp_ptr make_syntactic_closure();
Lisp_ptr capture_env();

Lisp_ptr proc_identifierp();
Lisp_ptr proc_identifier_eq();

Lisp_ptr gensym();
Lisp_ptr exit_func();

#endif // BUILTIN_EXTRA_HH
