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
Lisp_ptr syntax_and();
Lisp_ptr syntax_or();
Lisp_ptr syntax_cond();
Lisp_ptr whole_case();
Lisp_ptr whole_do();
Lisp_ptr macro_delay();
Lisp_ptr whole_function_quasiquote();

#endif // BUILTIN_SYNTAX_HH
