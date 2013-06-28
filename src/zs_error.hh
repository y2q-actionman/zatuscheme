#ifndef ZS_ERROR_HH
#define ZS_ERROR_HH

#include <string>
#include <cstdlib>
#include <cassert>
#include "decl.hh"
#include "lisp_ptr.hh"

std::string printf_string(const char*, ...)
  __attribute__ ((format (printf, 1, 2)))
  ;

#define UNEXP_DEFAULT() do{\
    assert(((void)"unexpected default case!", 0));      \
    abort();\
  }while(0)

Lisp_ptr zs_error(const std::string&);
Lisp_ptr zs_error(const std::string&, Lisp_ptr);

Lisp_ptr zs_error_append(const char*, Lisp_ptr);

Lisp_ptr builtin_type_check_failed(Ptr_tag, Lisp_ptr);
Lisp_ptr builtin_argcount_failed(const char*, int required, int max, int passed);
Lisp_ptr builtin_identifier_check_failed(Lisp_ptr);
Lisp_ptr builtin_range_check_failed(int max, int passed);
Lisp_ptr number_type_check_failed(Lisp_ptr);
Lisp_ptr procedure_type_check_failed(Lisp_ptr);

#endif // ZS_ERROR_HH
