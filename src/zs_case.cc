#include <cctype>

#include "zs_case.hh"

using namespace std;

#ifndef HAVE_STRCASECMP

int zs_strcasecmp(const char* s1, const char* s2){
  for(; *s1 && *s2; s1++, s2++){
    auto c1 = tolower(*s1);
    auto c2 = tolower(*s2);
    if(c1 != c2){
      return c1 - c2;
    }
  }

  return tolower(*s1) - tolower(*s2);
}

#endif // !defined HAVE_STRCASECMP

int zs_charcasecmp(char c1, char c2){
  char s1[2] = {c1, '\0'};
  char s2[2] = {c2, '\0'};
  return zs_strcasecmp(s1, s2);
}
