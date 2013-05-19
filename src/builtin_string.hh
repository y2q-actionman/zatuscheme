#ifndef BUILTIN_STRING_HH
#define BUILTIN_STRING_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr string_make(ZsArgs);
Lisp_ptr string_string(ZsArgs);
Lisp_ptr string_length(ZsArgs);
Lisp_ptr string_ref(ZsArgs);
Lisp_ptr string_set(ZsArgs);

Lisp_ptr internal_string_strcmp(ZsArgs);
Lisp_ptr internal_string_strcasecmp(ZsArgs);

Lisp_ptr string_append(ZsArgs);
Lisp_ptr string_to_list(ZsArgs);
Lisp_ptr string_from_list(ZsArgs);

}

#endif // BUILTIN_STRING_HH
