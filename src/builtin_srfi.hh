#ifndef BUILTIN_SRFI_HH
#define BUILTIN_SRFI_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr with_exception_handler(ZsArgs);
Lisp_ptr raise(ZsArgs);

} // namespace builtin

#endif // BUILTIN_SRFI_HH
