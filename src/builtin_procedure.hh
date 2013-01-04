#ifndef BUILTIN_PROCEDURE_HH
#define BUILTIN_PROCEDURE_HH

#include "decl.hh"

Lisp_ptr type_check_procedure();
Lisp_ptr apply_func();
Lisp_ptr func_force();
Lisp_ptr proc_values();
Lisp_ptr call_with_values();
Lisp_ptr call_cc();
Lisp_ptr dynamic_wind();

#endif // BUILTIN_PROCEDURE_HH
