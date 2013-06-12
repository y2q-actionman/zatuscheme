#ifndef BUILTIN_SYNTAX_HH
#define BUILTIN_SYNTAX_HH

#include "builtin.hh"

#define INTERNAL_UNDEFINED_SYMNAME "%undefined"

namespace builtin {

Lisp_ptr syntax_quote(ZsArgs);
Lisp_ptr syntax_lambda(ZsArgs);
Lisp_ptr syntax_if(ZsArgs);
Lisp_ptr syntax_set(ZsArgs);
Lisp_ptr syntax_define(ZsArgs);
Lisp_ptr syntax_begin(ZsArgs);
Lisp_ptr syntax_delay(ZsArgs);
Lisp_ptr syntax_quasiquote(ZsArgs);
Lisp_ptr syntax_unquote(ZsArgs);
Lisp_ptr syntax_unquote_splicing(ZsArgs);
Lisp_ptr syntax_define_syntax(ZsArgs);
Lisp_ptr syntax_syntax_rules(ZsArgs);

Lisp_ptr syntax_internal_memv(ZsArgs);

} 

#endif // BUILTIN_SYNTAX_HH
