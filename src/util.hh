#ifndef UTIL_HH
#define UTIL_HH

template<typename T, typename U>
inline
T c_cast(U u){
  return (T)u;
}

// type support
namespace zs {
  template<typename T> struct call_traits;
  template<typename T> struct call_traits_r;
}

#include "util.i.hh"

#endif // UTIL_HH
