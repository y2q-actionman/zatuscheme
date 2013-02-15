#ifndef ZS_ERROR_HH
#define ZS_ERROR_HH

#include <string>
#include <exception>
#include "decl.hh"
#include "lisp_ptr.hh"

std::string printf_string(const char*, ...)
  __attribute__ ((format (printf, 1, 2)))
  ;

// error classes
class zs_error : public std::exception{
protected:
  explicit zs_error();

public:
  explicit zs_error(const std::string&);
  explicit zs_error(std::string&&);
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
  zs_error_arg1(const std::string&, const char*, Lisp_ptr);
  zs_error_arg1(std::string&&, const char*, Lisp_ptr);
  zs_error_arg1(const zs_error_arg1&);
  zs_error_arg1(zs_error_arg1&&);

  virtual ~zs_error_arg1() noexcept;

  zs_error_arg1& operator=(const zs_error_arg1&) noexcept;
  zs_error_arg1& operator=(zs_error_arg1&&) noexcept;

  using zs_error::what;

private:
  const char* context_;
  Lisp_ptr arg_;
};


// error functions

void unexp_default(const char*, int)
__attribute__((noreturn))// [[noreturn]]
  ;

#define UNEXP_DEFAULT() unexp_default(__FILE__, __LINE__)

void unexp_conversion(const char*, int, const char*)
__attribute__((noreturn))// [[noreturn]]
  ;

#define UNEXP_CONVERSION(to) unexp_conversion(__FILE__, __LINE__, (to))


zs_error builtin_type_check_failed(const char*, Ptr_tag, Lisp_ptr);
zs_error builtin_argcount_failed(const char*, int required, int max, int passed);
zs_error builtin_identifier_check_failed(const char*, Lisp_ptr);

#endif // ZS_ERROR_HH
