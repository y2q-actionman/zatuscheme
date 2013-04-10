#ifndef BUILTIN_PROCEDURE_HH
#define BUILTIN_PROCEDURE_HH

#include "decl.hh"

namespace builtin {

Lisp_ptr procedurep();
Lisp_ptr apply();
Lisp_ptr force();
Lisp_ptr values();
Lisp_ptr call_with_values();
Lisp_ptr call_cc();
Lisp_ptr dynamic_wind();

}

#endif // BUILTIN_PROCEDURE_HH
