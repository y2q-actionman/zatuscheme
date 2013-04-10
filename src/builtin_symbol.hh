#ifndef BUILTIN_SYMBOL_HH
#define BUILTIN_SYMBOL_HH

#include "decl.hh"

namespace builtin {

Lisp_ptr symbol_to_string();
Lisp_ptr symbol_from_string();

}

#endif // BUILTIN_SYMBOL_HH
