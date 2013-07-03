#include "zs_case.hh"

#ifndef HAVE_STRCASECMP

int zs_strcasecmp(const char* s1, const char* s2){
  for(; *s1 && *s2; s1++, s2++){
    auto c1 = ZS_CASE(*s1);
    auto c2 = ZS_CASE(*s2);
    if(c1 != c2){
      return c1 - c2;
    }
  }

  return ZS_CASE(*s1) - ZS_CASE(*s2);
}

#endif // HAVE_STRCASECMP
