#ifndef BUILTIN_CONS_HH
#define BUILTIN_CONS_HH

#include "builtin.hh"

namespace zs {
namespace builtin {

Lisp_ptr cons_pairp(ZsArgs);
Lisp_ptr cons_cons(ZsArgs);
Lisp_ptr cons_car(ZsArgs);
Lisp_ptr cons_cdr(ZsArgs);
Lisp_ptr cons_set_car(ZsArgs);
Lisp_ptr cons_set_cdr(ZsArgs);

} // namespace builtin
} // namespace zs

#endif // BUILTIN_CONS_HH
