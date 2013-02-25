#ifndef ZS_ERROR_HH
#define ZS_ERROR_HH

#include <string>
#include <exception>
#include <initializer_list>
#include <array>
#include <cstdlib>
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
  static const size_t ARGS_SIZE = 3;

  zs_error_arg1(const char*, const std::string&,
                std::initializer_list<Lisp_ptr>);
  zs_error_arg1(const char*, const std::string&);
  // zs_error_arg1(std::string&&, const char*, Lisp_ptr);
  zs_error_arg1(const zs_error_arg1&);
  zs_error_arg1(zs_error_arg1&&);

  virtual ~zs_error_arg1() noexcept;

  zs_error_arg1& operator=(const zs_error_arg1&) noexcept;
  zs_error_arg1& operator=(zs_error_arg1&&) noexcept;

  using zs_error::what; // override

private:
  const char* context_;
  std::string body_;
  std::array<Lisp_ptr, ARGS_SIZE> args_;
};


// error functions

#define UNEXP_DEFAULT() do{\
    assert(((void)"unexpected default case!", 0));      \
    abort();\
  }while(0)

#define UNEXP_CONVERSION(to) do{\
    assert(((void)"unexpected conversion to "to"!", 0));\
    abort();\
  }while(0)

zs_error builtin_type_check_failed(const char*, Ptr_tag, Lisp_ptr);
zs_error builtin_argcount_failed(const char*, int required, int max, int passed);
zs_error builtin_identifier_check_failed(const char*, Lisp_ptr);

#endif // ZS_ERROR_HH
