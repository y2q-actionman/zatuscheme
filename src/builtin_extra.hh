#ifndef BUILTIN_EXTRA_HH
#define BUILTIN_EXTRA_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr transcript_on(ZsArgs);
Lisp_ptr transcript_off(ZsArgs);

// macros
Lisp_ptr traditional_transformer(ZsArgs);
Lisp_ptr gensym(ZsArgs);

Lisp_ptr sc_macro_transformer(ZsArgs);
Lisp_ptr make_syntactic_closure(ZsArgs);
Lisp_ptr current_environment(ZsArgs);

Lisp_ptr identifierp(ZsArgs);
Lisp_ptr identifier_eq(ZsArgs);
Lisp_ptr make_empty_environment(ZsArgs);

// error functions
Lisp_ptr push_exception_handler(ZsArgs);
Lisp_ptr pop_exception_handler(ZsArgs);
Lisp_ptr raise(ZsArgs);

// extensions
Lisp_ptr exit(ZsArgs);
Lisp_ptr hard_repl(ZsArgs);
Lisp_ptr tmp_file(ZsArgs);
}

#endif // BUILTIN_EXTRA_HH
