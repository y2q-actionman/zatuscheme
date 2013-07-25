#ifndef BUILTIN_PROCEDURE_HH
#define BUILTIN_PROCEDURE_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr procedurep(ZsArgs);
Lisp_ptr apply(ZsArgs);
Lisp_ptr values(ZsArgs);
Lisp_ptr call_with_values(ZsArgs);
Lisp_ptr call_cc(ZsArgs);
Lisp_ptr internal_push_winding(ZsArgs);
Lisp_ptr internal_pop_winding(ZsArgs);

}

#endif // BUILTIN_PROCEDURE_HH
