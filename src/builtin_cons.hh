#ifndef BUILTIN_CONS_HH
#define BUILTIN_CONS_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr cons_pairp(ZsArgs);
Lisp_ptr cons_cons(ZsArgs);
Lisp_ptr cons_car(ZsArgs);
Lisp_ptr cons_cdr(ZsArgs);
Lisp_ptr cons_set_car(ZsArgs);
Lisp_ptr cons_set_cdr(ZsArgs);
Lisp_ptr cons_listp(ZsArgs);
Lisp_ptr cons_list(ZsArgs);
Lisp_ptr cons_list_star(ZsArgs);
Lisp_ptr cons_append(ZsArgs);

}

#endif // BUILTIN_CONS_HH
