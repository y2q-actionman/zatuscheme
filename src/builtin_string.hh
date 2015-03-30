#ifndef BUILTIN_STRING_HH
#define BUILTIN_STRING_HH

#include "builtin.hh"

namespace zs {
namespace builtin {

Lisp_ptr string_make(ZsArgs);
Lisp_ptr string_length(ZsArgs);
Lisp_ptr string_ref(ZsArgs);
Lisp_ptr string_set(ZsArgs);

Lisp_ptr string_strcmp(ZsArgs);
Lisp_ptr string_strcasecmp(ZsArgs);

}
}

#endif // BUILTIN_STRING_HH
