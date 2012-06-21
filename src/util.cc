#include <cstdlib>
#include <cstdio>

#include "util.hh"

using namespace std;

void
unexp_default(const char* f, int l){
  fprintf(stderr, "unexpected default case! (file=%s, line=%d)\n", f, l);
  abort();
}

void
unexp_conversion(const char* f, int l, const char* to){
  fprintf(stderr, "unexpected conversion to %s! (file=%s, line=%d)\n",
          to, f, l);
  abort();
}
