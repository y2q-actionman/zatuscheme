#include <cctype>

#include "config.h"
#include "zs_case.hh"

#if HAVE_STRCASECMP
# include <strings.h>
#endif

namespace zs {

int zs_strcasecmp(const char* s1, const char* s2){
#if HAVE_STRCASECMP
  return strcasecmp(s1, s2);
#else
  for(; *s1 && *s2; s1++, s2++){
    auto c1 = std::tolower(*s1);
    auto c2 = std::tolower(*s2);
    if(c1 != c2){
      return c1 - c2;
    }
  }
  return std::tolower(*s1) - std::tolower(*s2);
#endif // HAVE_STRCASECMP
}

int zs_charcasecmp(char c1, char c2){
  char s1[2] = {c1, '\0'};
  char s2[2] = {c2, '\0'};
  return zs_strcasecmp(s1, s2);
}

} // namespace zs
