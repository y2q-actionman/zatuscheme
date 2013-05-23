#ifndef BUILTIN_SRFI_HH
#define BUILTIN_SRFI_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr error(ZsArgs);
Lisp_ptr with_exception_handler(ZsArgs);
Lisp_ptr raise(ZsArgs);
Lisp_ptr unwind(ZsArgs);

} // namespace builtin

#endif // BUILTIN_SRFI_HH
