#ifndef BUILTIN_PROCEDURE_HH
#define BUILTIN_PROCEDURE_HH

#include "decl.hh"

Lisp_ptr type_check_procedure();
Lisp_ptr proc_values();

extern const char* builtin_procedure_load[];
extern const size_t builtin_procedure_load_size;

#endif // BUILTIN_PROCEDURE_HH
