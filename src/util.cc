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

// class zs_error
zs_error::zs_error(const std::string& s)
: str_(s){}

zs_error::zs_error(const zs_error&) = default;

zs_error::~zs_error() noexcept = default;

zs_error& zs_error::operator=(const zs_error&) noexcept = default;

const char* zs_error::what() const noexcept{
  return str_.c_str();
}
