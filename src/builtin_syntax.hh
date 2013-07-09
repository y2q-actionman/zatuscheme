#ifndef BUILTIN_SYNTAX_HH
#define BUILTIN_SYNTAX_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr syntax_quote(ZsArgs);
Lisp_ptr syntax_lambda(ZsArgs);
Lisp_ptr syntax_if(ZsArgs);
Lisp_ptr syntax_set(ZsArgs);
Lisp_ptr syntax_define(ZsArgs);
Lisp_ptr syntax_internal_quasiquote_list(ZsArgs);
Lisp_ptr syntax_internal_quasiquote_vector(ZsArgs);
Lisp_ptr syntax_unquote_splicing(ZsArgs);
Lisp_ptr syntax_syntax_rules(ZsArgs);

Lisp_ptr syntax_internal_memv(ZsArgs);
Lisp_ptr syntax_internal_list_star(ZsArgs);
Lisp_ptr syntax_internal_vector(ZsArgs);

} 

#endif // BUILTIN_SYNTAX_HH
