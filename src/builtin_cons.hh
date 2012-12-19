#ifndef BUILTIN_CONS_HH
#define BUILTIN_CONS_HH

#include "lisp_ptr.hh"

extern const char* builtin_cons_load[];
extern const size_t builtin_cons_load_size;

Lisp_ptr type_check_pair();
Lisp_ptr cons_cons();
Lisp_ptr cons_car();
Lisp_ptr cons_cdr();
Lisp_ptr cons_set_car();
Lisp_ptr cons_set_cdr();
Lisp_ptr cons_nullp();
Lisp_ptr cons_listp();
Lisp_ptr cons_list();
Lisp_ptr cons_list_star();
Lisp_ptr cons_length();
Lisp_ptr cons_append();
Lisp_ptr cons_reverse();
Lisp_ptr cons_list_tail();
Lisp_ptr cons_list_ref();

#endif // BUILTIN_CONS_HH
