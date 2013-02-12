#ifndef ZS_ERROR_HH
#define ZS_ERROR_HH

#include <string>
#include <exception>
#include "decl.hh"

void
__attribute__((noreturn))// [[noreturn]]
unexp_default(const char*, int);

#define UNEXP_DEFAULT() unexp_default(__FILE__, __LINE__)

void
__attribute__((noreturn))// [[noreturn]]
unexp_conversion(const char*, int, const char*);

#define UNEXP_CONVERSION(to) unexp_conversion(__FILE__, __LINE__, (to))


class zs_error : public std::exception{
protected:
  explicit zs_error();

public:
  explicit zs_error(const std::string&);
  explicit zs_error(std::string&&);
  explicit zs_error(const char*, ...)
    __attribute__ ((format (printf, 2, 3)))
    ;
  zs_error(const zs_error&);
  zs_error(zs_error&&);

  virtual ~zs_error() noexcept;

  zs_error& operator=(const zs_error&) noexcept;
  zs_error& operator=(zs_error&&) noexcept;

  virtual const char* what() const noexcept; // override

protected:
  std::string str_;
};


class zs_error_arg1 : public zs_error{
public:
  explicit zs_error_arg1(const char*, Lisp_ptr, const char*, ...)
    __attribute__ ((format (printf, 4, 5)))
    ;
  zs_error_arg1(const zs_error_arg1&);
  zs_error_arg1(zs_error_arg1&&);

  virtual ~zs_error_arg1() noexcept;

  zs_error_arg1& operator=(const zs_error_arg1&) noexcept;
  zs_error_arg1& operator=(zs_error_arg1&&) noexcept;

  using zs_error::what;
};

// builtin type checking
zs_error builtin_type_check_failed(const char*, Ptr_tag, Lisp_ptr);
zs_error builtin_argcount_failed(const char*, int required, int max, int passed);
zs_error builtin_identifier_check_failed(const char*, Lisp_ptr);

#endif // ZS_ERROR_HH
