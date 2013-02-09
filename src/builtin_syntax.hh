#ifndef BUILTIN_SYNTAX_HH
#define BUILTIN_SYNTAX_HH

#include "decl.hh"

Lisp_ptr whole_function_error();
Lisp_ptr whole_function_pass_through();
Lisp_ptr syntax_quote();
Lisp_ptr syntax_lambda();
Lisp_ptr syntax_if();
Lisp_ptr syntax_set();
Lisp_ptr syntax_define();
Lisp_ptr syntax_begin();
Lisp_ptr syntax_let();
Lisp_ptr syntax_let_star();
Lisp_ptr syntax_letrec();
Lisp_ptr syntax_do();
Lisp_ptr syntax_delay();
Lisp_ptr syntax_quasiquote();
Lisp_ptr syntax_unquote();
Lisp_ptr syntax_unquote_splicing();
Lisp_ptr syntax_else();
Lisp_ptr syntax_arrow();
Lisp_ptr syntax_define_syntax();
Lisp_ptr syntax_let_syntax();
Lisp_ptr syntax_letrec_syntax();
Lisp_ptr syntax_syntax_rules();

#endif // BUILTIN_SYNTAX_HH
