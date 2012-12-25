#include <cstdio>
#include <utility>
#include <cstdarg>

#include "util.hh"

using namespace std;

void unexp_default(const char* f, int l){
  throw make_zs_error("unexpected default case! (file=%s, line=%d)\n", f, l);
}

void unexp_conversion(const char* f, int l, const char* to){
  throw make_zs_error("unexpected conversion to %s! (file=%s, line=%d)\n",
                      to, f, l);
}

// class zs_error
zs_error::zs_error(const std::string& s) : str_(s){}
zs_error::zs_error(std::string&& s) : str_(std::move(s)){}

zs_error::zs_error(const char* fmt, ...) : str_(){
  char tmp[256];

  va_list ap;
  va_start(ap, fmt);
  auto len = vsnprintf(tmp, sizeof(tmp), fmt, ap);
  va_end(ap);

  str_ = std::string(tmp, len);
}

zs_error::zs_error(const zs_error&) = default;
zs_error::zs_error(zs_error&&) = default;

zs_error::~zs_error() noexcept = default;

zs_error& zs_error::operator=(const zs_error&) noexcept = default;
zs_error& zs_error::operator=(zs_error&&) noexcept = default;

const char* zs_error::what() const noexcept{
  return str_.c_str();
}

/*
zs_error make_zs_error(const char* fmt, ...){
  char tmp[256];

  va_list ap;
  va_start(ap, fmt);
  auto len = vsnprintf(tmp, sizeof(tmp), fmt, ap);
  va_end(ap);

  return zs_error({tmp, len});
}
*/
