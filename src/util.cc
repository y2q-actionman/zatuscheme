#include <cstdlib>
#include <cstdio>

#include "util.hh"

using namespace std;

namespace zs{
  FILE* in = stdin;
  FILE* out = stdout;
  FILE* err = stderr;
}

void
unexp_default(const char* f, int l){
  fprintf(zs::err, "unexpected default case! (file=%s, line=%d)\n", f, l);
  abort();
}

void
unexp_conversion(const char* f, int l, const char* to){
  fprintf(zs::err, "unexpected conversion to %s! (file=%s, line=%d)\n",
          to, f, l);
  abort();
}
