#ifndef BUILTIN_CHAR_HH
#define BUILTIN_CHAR_HH

#include "decl.hh"

Lisp_ptr char_eq();
Lisp_ptr char_less();
Lisp_ptr char_greater();
Lisp_ptr char_less_eq();
Lisp_ptr char_greater_eq();
Lisp_ptr char_ci_eq();
Lisp_ptr char_ci_less();
Lisp_ptr char_ci_greater();
Lisp_ptr char_ci_less_eq();
Lisp_ptr char_ci_greater_eq();
Lisp_ptr char_isalpha();
Lisp_ptr char_isdigit();
Lisp_ptr char_isspace();
Lisp_ptr char_isupper();
Lisp_ptr char_islower();
Lisp_ptr char_to_int();
Lisp_ptr char_from_int();
Lisp_ptr char_toupper();
Lisp_ptr char_tolower();

#endif // BUILTIN_CHAR_HH
