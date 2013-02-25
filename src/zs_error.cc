#include <cstdio>
#include <utility>
#include <cstdarg>
#include <sstream>
#include <cassert>

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
zs_error_arg1::zs_error_arg1(const char* context, const std::string& body,
                             std::initializer_list<Lisp_ptr> args)
  : zs_error(), context_(context), body_(body), args_()
{
  assert(args.size() <= ARGS_SIZE);

  int i = 0;
  for(auto a : args){
    args_.at(i) = a;
  }

  ostringstream oss;

  oss << context_ << " : " << body_;
  if(args_[0]){
    oss << " @ (";
    for(auto p : args_){
      oss << p << " ";
    }
    oss << ")";
  }
  oss << endl;
  
  this->str_ = oss.str();
}

// should use 'delegating constructor'
zs_error_arg1::zs_error_arg1(const char* context, const std::string& body)
  : zs_error(), context_(context), body_(body), args_()
{
  ostringstream oss;
  oss << context_ << " : " << body_ << endl;
  this->str_ = oss.str();
}

zs_error_arg1::zs_error_arg1(const zs_error_arg1&) = default;
zs_error_arg1::zs_error_arg1(zs_error_arg1&&) = default;

zs_error_arg1::~zs_error_arg1() noexcept = default;

zs_error_arg1& zs_error_arg1::operator=(const zs_error_arg1&) noexcept = default;
zs_error_arg1& zs_error_arg1::operator=(zs_error_arg1&&) noexcept = default;


// error functions
zs_error builtin_type_check_failed(const char* func_name, Ptr_tag tag, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       printf_string("arg is not %s!", stringify(tag)),
                       {p});
}

zs_error builtin_argcount_failed(const char* name, int required, int max, int passed){
  throw zs_error_arg1(name,
                      printf_string("number of passed args is mismatched!!"
                                    " (acceptable %d-%d args, passed %d)\n",
                                    required, max, passed));
}

zs_error builtin_identifier_check_failed(const char* name, Lisp_ptr p){
  return zs_error_arg1(name,
                       "arg is not identifier!",
                       {p});
}
