#ifndef ZS_ERROR_HH
#define ZS_ERROR_HH

#include <string>
#include <exception>

void
__attribute__((noreturn))// [[noreturn]]
unexp_default(const char*, int);

#define UNEXP_DEFAULT() unexp_default(__FILE__, __LINE__)

void
__attribute__((noreturn))// [[noreturn]]
unexp_conversion(const char*, int, const char*);

#define UNEXP_CONVERSION(to) unexp_conversion(__FILE__, __LINE__, (to))


class zs_error : public std::exception{
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

private:
  std::string str_;
};

#endif // ZS_ERROR_HH
