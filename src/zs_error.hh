#ifndef ZS_ERROR_HH
#define ZS_ERROR_HH

#include <string>
#include <initializer_list>
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

#define UNEXP_CONVERSION(to) do{\
    assert(((void)"unexpected conversion to "to"!", 0));\
    abort();\
  }while(0)

Lisp_ptr zs_error(const std::string&);

Lisp_ptr zs_error_arg1(const char* context, const std::string& str,
                       std::initializer_list<Lisp_ptr>);
Lisp_ptr zs_error_arg1(const char* context, const std::string& str);

Lisp_ptr builtin_type_check_failed(const char*, Ptr_tag, Lisp_ptr);
Lisp_ptr builtin_argcount_failed(const char*, int required, int max, int passed);
Lisp_ptr builtin_identifier_check_failed(const char*, Lisp_ptr);
Lisp_ptr builtin_range_check_failed(int max, int passed);

#endif // ZS_ERROR_HH
