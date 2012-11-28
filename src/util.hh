#ifndef UTIL_HH
#define UTIL_HH

#include <string>
#include <exception>

template<typename T, typename U>
inline
T c_cast(U u){
  return (T)u;
}

// error report
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
  zs_error(const zs_error&);
  zs_error(zs_error&&);

  virtual ~zs_error() noexcept;

  zs_error& operator=(const zs_error&) noexcept;
  zs_error& operator=(zs_error&&) noexcept;

  virtual const char* what() const noexcept; // override

private:
  std::string str_;
};

zs_error make_zs_error(const char*, ...)
  __attribute__ ((format (printf, 1, 2)))
  ;

// type support
namespace zs {
  template<typename T> struct call_traits;
  template<typename T> struct call_traits_r;
}

#include "util.i.hh"

#endif // UTIL_HH
