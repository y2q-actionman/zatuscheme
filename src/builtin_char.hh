#ifndef BUILTIN_CHAR_HH
#define BUILTIN_CHAR_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr internal_char_casecmp(ZsArgs);

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
