#ifndef BUILTIN_EXTRA_HH
#define BUILTIN_EXTRA_HH

#include "decl.hh"

namespace builtin {

Lisp_ptr traditional_transformer();
Lisp_ptr gensym();

Lisp_ptr sc_macro_transformer();

Lisp_ptr make_syntactic_closure();
Lisp_ptr capture_syntactic_environment();

Lisp_ptr identifierp();
Lisp_ptr identifier_eq();
Lisp_ptr make_synthetic_identifier();

Lisp_ptr exit();

Lisp_ptr transcript_on();
Lisp_ptr transcript_off();

}

#endif // BUILTIN_EXTRA_HH
