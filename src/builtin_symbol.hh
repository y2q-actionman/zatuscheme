#ifndef BUILTIN_SYMBOL_HH
#define BUILTIN_SYMBOL_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr symbol_to_string(ZsArgs);
Lisp_ptr symbol_from_string(ZsArgs);

}

#endif // BUILTIN_SYMBOL_HH
