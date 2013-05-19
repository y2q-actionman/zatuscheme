#ifndef BUILTIN_STRING_HH
#define BUILTIN_STRING_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr string_make(ZsArgs);
Lisp_ptr string_string(ZsArgs);
Lisp_ptr string_length(ZsArgs);
Lisp_ptr string_ref(ZsArgs);
Lisp_ptr string_set(ZsArgs);

Lisp_ptr string_equal(ZsArgs);
Lisp_ptr string_less(ZsArgs);
Lisp_ptr string_greater(ZsArgs);
Lisp_ptr string_less_eq(ZsArgs);
Lisp_ptr string_greater_eq(ZsArgs);

Lisp_ptr string_ci_equal(ZsArgs);
Lisp_ptr string_ci_less(ZsArgs);
Lisp_ptr string_ci_greater(ZsArgs);
Lisp_ptr string_ci_less_eq(ZsArgs);
Lisp_ptr string_ci_greater_eq(ZsArgs);

Lisp_ptr string_substr(ZsArgs);
Lisp_ptr string_append(ZsArgs);
Lisp_ptr string_to_list(ZsArgs);
Lisp_ptr string_from_list(ZsArgs);
Lisp_ptr string_copy(ZsArgs);

}

#endif // BUILTIN_STRING_HH
