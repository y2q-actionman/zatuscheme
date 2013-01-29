#include <cstdio>
#include <utility>
#include <cstdarg>
#include <sstream>

#include "zs_error.hh"
#include "lisp_ptr.hh"
#include "printer.hh"

static const size_t ERROR_MESSAGE_LENGTH = 256;

using namespace std;

void unexp_default(const char* f, int l){
  throw zs_error("unexpected default case! (file=%s, line=%d)\n", f, l);
}

void unexp_conversion(const char* f, int l, const char* to){
  throw zs_error("unexpected conversion to %s! (file=%s, line=%d)\n",
                 to, f, l);
}

// class zs_error
zs_error::zs_error() : str_(){}
zs_error::zs_error(const std::string& s) : str_(s){}
zs_error::zs_error(std::string&& s) : str_(std::move(s)){}

zs_error::zs_error(const char* fmt, ...) : str_(ERROR_MESSAGE_LENGTH, '\0'){
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(&(str_[0]), sizeof(str_), fmt, ap);
  va_end(ap);
}

zs_error::zs_error(const zs_error&) = default;
zs_error::zs_error(zs_error&&) = default;

zs_error::~zs_error() noexcept = default;

zs_error& zs_error::operator=(const zs_error&) noexcept = default;
zs_error& zs_error::operator=(zs_error&&) noexcept = default;

const char* zs_error::what() const noexcept{
  return str_.c_str();
}


// class zs_error_arg1
zs_error_arg1::zs_error_arg1(const char* name, Lisp_ptr p, const char* fmt, ...)
  : zs_error(){
  stringstream ss;

  ss << name << ": ";

  char tmp[256];
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(tmp, sizeof(tmp), fmt, ap);
  ss << tmp;
  va_end(ap);

  ss << "\n\targ: " << p << '\n';

  str_ = ss.str(); // dirty!
}

zs_error_arg1::zs_error_arg1(const zs_error_arg1&) = default;
zs_error_arg1::zs_error_arg1(zs_error_arg1&&) = default;

zs_error_arg1::~zs_error_arg1() noexcept = default;

zs_error_arg1& zs_error_arg1::operator=(const zs_error_arg1&) noexcept = default;
zs_error_arg1& zs_error_arg1::operator=(zs_error_arg1&&) noexcept = default;
