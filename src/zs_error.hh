#ifndef ZS_ERROR_HH
#define ZS_ERROR_HH

#include <cassert>
#include <climits>
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

void print_zs_warning(const char*, ...)
  __attribute__ ((format (printf, 1, 2)))
  ;

void check_type(Ptr_tag, Lisp_ptr);
void check_numeric_type(Lisp_ptr);
void check_identifier_type(Lisp_ptr);
void check_procedure_type(Lisp_ptr);
void check_range(Lisp_ptr, size_t, size_t = INT_MAX);

#endif // ZS_ERROR_HH
