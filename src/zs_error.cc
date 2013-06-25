#include <cstdio>
#include <utility>
#include <cstdarg>
#include <sstream>
#include <cassert>

#include "zs_error.hh"
#include "zs_memory.hh"
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

// error functions
Lisp_ptr zs_error_arg1(const char* context, const std::string& str,
                       std::initializer_list<Lisp_ptr> args){
  ostringstream oss;

  if(context){
    oss << context << " : ";
  }
  oss << str;
  if(args.size() > 0){
    oss << " @ (";
    for(auto p : args){
      oss << p << " ";
    }
    oss << ")";
  }
  oss << endl;
  
  return zs_new<String>(oss.str());
}

Lisp_ptr zs_error_arg1(const char* context, const std::string& str){
  return zs_error_arg1(context, str, {});
}

Lisp_ptr builtin_type_check_failed(const char* func_name, Ptr_tag tag, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       printf_string("arg is not %s!", stringify(tag)),
                       {p});
}

Lisp_ptr builtin_argcount_failed(const char* name, int required, int max, int passed){
  return zs_error_arg1(name,
                       printf_string("number of passed args is mismatched!!"
                                     " (acceptable %d-%d args, passed %d)\n",
                                    required, max, passed));
}

Lisp_ptr builtin_identifier_check_failed(const char* name, Lisp_ptr p){
  return zs_error_arg1(name,
                       "arg is not identifier!",
                       {p});
}

zs_error builtin_range_check_failed(int max, int passed){
  return zs_error(printf_string("index is out-of-bound ([0, %d), supplied %d\n",
                                max, passed));
}
