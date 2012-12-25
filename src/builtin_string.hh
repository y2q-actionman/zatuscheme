#ifndef BUILTIN_STRING_HH
#define BUILTIN_STRING_HH

#include "decl.hh"

Lisp_ptr string_make();
Lisp_ptr string_string();
Lisp_ptr string_length();
Lisp_ptr string_ref();
Lisp_ptr string_set();

Lisp_ptr string_equal();
Lisp_ptr string_less();
Lisp_ptr string_greater();
Lisp_ptr string_less_eq();
Lisp_ptr string_greater_eq();

Lisp_ptr string_ci_equal();
Lisp_ptr string_ci_less();
Lisp_ptr string_ci_greater();
Lisp_ptr string_ci_less_eq();
Lisp_ptr string_ci_greater_eq();

Lisp_ptr string_substr();
Lisp_ptr string_append();
Lisp_ptr string_to_list();
Lisp_ptr string_from_list();
Lisp_ptr string_copy();
Lisp_ptr string_fill();

#endif // BUILTIN_STRING_HH
