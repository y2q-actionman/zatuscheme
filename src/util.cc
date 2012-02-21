#include <cstdlib>
#include <cstdio>

#include "util.hh"

using namespace std;

void
unexp_default(const char* f, int l){
  fprintf(stderr, "unexpected default case! (file=%s, line=%d)", f, l);
  abort();
}
