#ifndef BUILTIN_CHAR_HH
#define BUILTIN_CHAR_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr char_eq(ZsArgs);
Lisp_ptr char_less(ZsArgs);
Lisp_ptr char_greater(ZsArgs);
Lisp_ptr char_less_eq(ZsArgs);
Lisp_ptr char_greater_eq(ZsArgs);
Lisp_ptr char_ci_eq(ZsArgs);
Lisp_ptr char_ci_less(ZsArgs);
Lisp_ptr char_ci_greater(ZsArgs);
Lisp_ptr char_ci_less_eq(ZsArgs);
Lisp_ptr char_ci_greater_eq(ZsArgs);
Lisp_ptr char_isalpha(ZsArgs);
Lisp_ptr char_isdigit(ZsArgs);
Lisp_ptr char_isspace(ZsArgs);
Lisp_ptr char_isupper(ZsArgs);
Lisp_ptr char_islower(ZsArgs);
Lisp_ptr char_to_int(ZsArgs);
Lisp_ptr char_from_int(ZsArgs);
Lisp_ptr char_toupper(ZsArgs);
Lisp_ptr char_tolower(ZsArgs);

}

#endif // BUILTIN_CHAR_HH
