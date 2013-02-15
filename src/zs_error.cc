#include <cstdio>
#include <utility>
#include <cstdarg>
#include <sstream>

#include "zs_error.hh"
#include "lisp_ptr.hh"
#include "printer.hh"

static const size_t ERROR_MESSAGE_LENGTH = 256;

using namespace std;

std::string printf_string(const char* fmt, ...){
  string str(ERROR_MESSAGE_LENGTH, '\0');

  va_list ap;
  va_start(ap, fmt);
  vsnprintf(&(str[0]), str.size(), fmt, ap);
  va_end(ap);

  return str;
}

// class zs_error
zs_error::zs_error() : str_(){}
zs_error::zs_error(const std::string& s) : str_(s){}
zs_error::zs_error(std::string&& s) : str_(std::move(s)){}

zs_error::zs_error(const zs_error&) = default;
zs_error::zs_error(zs_error&&) = default;

zs_error::~zs_error() noexcept = default;

zs_error& zs_error::operator=(const zs_error&) noexcept = default;
zs_error& zs_error::operator=(zs_error&&) noexcept = default;

const char* zs_error::what() const noexcept{
  return str_.c_str();
}


// class zs_error_arg1
zs_error_arg1::zs_error_arg1(const std::string& estr, const char* context, Lisp_ptr p)
  : zs_error(estr), context_(context), arg_(p){}

zs_error_arg1::zs_error_arg1(std::string&& estr, const char* context, Lisp_ptr p)
  : zs_error(move(estr)), context_(context), arg_(p){}

zs_error_arg1::zs_error_arg1(const zs_error_arg1&) = default;
zs_error_arg1::zs_error_arg1(zs_error_arg1&&) = default;

zs_error_arg1::~zs_error_arg1() noexcept = default;

zs_error_arg1& zs_error_arg1::operator=(const zs_error_arg1&) noexcept = default;
zs_error_arg1& zs_error_arg1::operator=(zs_error_arg1&&) noexcept = default;


// error functions
void unexp_default(const char* f, int l){
  throw zs_error(printf_string("unexpected default case! (file=%s, line=%d)\n", f, l));
}

void unexp_conversion(const char* f, int l, const char* to){
  throw zs_error(printf_string("unexpected conversion to %s! (file=%s, line=%d)\n",
                               to, f, l));
}

zs_error builtin_type_check_failed(const char* func_name, Ptr_tag tag, Lisp_ptr p){
  return zs_error(printf_string("native func: %s: arg is not %s! (%s)\n",
                                func_name, stringify(tag), stringify(p.tag())));
}

zs_error builtin_argcount_failed(const char* name, int required, int max, int passed){
  throw zs_error(printf_string("eval error: %s: number of passed args is mismatched!!"
                               " (acceptable %d-%d args, passed %d)\n",
                               name, required, max, passed));
}

zs_error builtin_identifier_check_failed(const char* name, Lisp_ptr p){
  return zs_error(printf_string("eval error: %s: arg is not identifier! (%s)\n",
                                name, stringify(p.tag())));
}
