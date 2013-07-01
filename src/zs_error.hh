#ifndef ZS_ERROR_HH
#define ZS_ERROR_HH

#include <cstdlib>
#include <cassert>
#include "decl.hh"

#define UNEXP_DEFAULT() do{\
    assert(((void)"unexpected default case!", 0));      \
    abort();\
  }while(0)

void throw_zs_error(Lisp_ptr, const char*, ...)
  __attribute__ ((noreturn, format (printf, 2, 3)))
  ;

void throw_zs_error_append(const char*, Lisp_ptr)
  __attribute__ ((noreturn))
  ;

void throw_builtin_type_check_failed(Ptr_tag, Lisp_ptr)
  __attribute__ ((noreturn))
  ;
void throw_builtin_argcount_failed(const char*, int required, int max, int passed)
  __attribute__ ((noreturn))
  ;
void throw_builtin_identifier_check_failed(Lisp_ptr)
  __attribute__ ((noreturn))
  ;
void throw_builtin_range_check_failed(int max, int passed)
  __attribute__ ((noreturn))
  ;
void throw_number_type_check_failed(Lisp_ptr)
  __attribute__ ((noreturn))
  ;
void throw_procedure_type_check_failed(Lisp_ptr)
  __attribute__ ((noreturn))
  ;

#endif // ZS_ERROR_HH
