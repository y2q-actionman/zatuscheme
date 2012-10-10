#ifndef UTIL_HH
#define UTIL_HH

#include <cstdio>

template<typename T, typename U>
inline
T c_cast(U u){
  return (T)u;
}

// io
namespace zs{
  extern FILE* in;
  extern FILE* out;
  extern FILE* err;
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

// type support
namespace zs {
  template<typename T> struct call_traits;
  template<typename T> struct call_traits_r;
}

#include "util.i.hh"

#endif // UTIL_HH
