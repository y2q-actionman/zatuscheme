#ifndef BUILTIN_EXTRA_HH
#define BUILTIN_EXTRA_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr traditional_transformer(ZsArgs);
Lisp_ptr gensym(ZsArgs);

Lisp_ptr sc_macro_transformer(ZsArgs);

Lisp_ptr make_syntactic_closure(ZsArgs);
Lisp_ptr capture_syntactic_environment(ZsArgs);

Lisp_ptr identifierp(ZsArgs);
Lisp_ptr identifier_eq(ZsArgs);
Lisp_ptr make_synthetic_identifier(ZsArgs);

Lisp_ptr exit(ZsArgs);

Lisp_ptr transcript_on(ZsArgs);
Lisp_ptr transcript_off(ZsArgs);

Lisp_ptr hard_repl(ZsArgs);

Lisp_ptr tmp_file(ZsArgs);
}

#endif // BUILTIN_EXTRA_HH
