#ifndef ZS_ERROR_HH
#define ZS_ERROR_HH

#include <cassert>
#include <cstdlib>
#include "decl.hh"

#define UNEXP_DEFAULT() do{\
    assert(((void)"unexpected default case!", 0));      \
    abort();\
  }while(0)

void zs_terminate_handler() noexcept;

void throw_zs_error(Lisp_ptr, const char*, ...)
  __attribute__ ((noreturn, format (printf, 2, 3)))
  ;

void throw_zs_error_append(Lisp_ptr, Lisp_ptr)
  __attribute__ ((noreturn))
  ;

void throw_builtin_type_check_failed(Ptr_tag, Lisp_ptr)
  __attribute__ ((noreturn))
  ;
void throw_builtin_argcount_failed(Lisp_ptr, int required, int max, int passed)
  __attribute__ ((noreturn))
  ;
void throw_builtin_identifier_check_failed(Lisp_ptr)
  __attribute__ ((noreturn))
  ;
void throw_builtin_range_check_failed(size_t max, int passed)
  __attribute__ ((noreturn))
  ;
void throw_number_type_check_failed(Lisp_ptr)
  __attribute__ ((noreturn))
  ;
void throw_procedure_type_check_failed(Lisp_ptr)
  __attribute__ ((noreturn))
  ;

void print_zs_warning(const char*, ...)
  __attribute__ ((format (printf, 1, 2)))
  ;

void check_type(Ptr_tag, Lisp_ptr);
void check_numeric_type(Lisp_ptr);
void check_identifier_type(Lisp_ptr);

#endif // ZS_ERROR_HH
