#include <cstdio>
#include <utility>
#include <cstdarg>
#include <sstream>
#include <cassert>
#include <string>

#include "zs_error.hh"
#include "zs_memory.hh"
#include "lisp_ptr.hh"
#include "printer.hh"

static const size_t ERROR_MESSAGE_LENGTH = 256;

using namespace std;

// error functions
void throw_zs_error(Lisp_ptr p, const char* fmt, ...){
  string str(ERROR_MESSAGE_LENGTH, '\0');

  va_list ap;
  va_start(ap, fmt);
  vsnprintf(&(str[0]), str.size(), fmt, ap);
  va_end(ap);

  if(!p){
    throw Lisp_ptr{zs_new<String>(move(str))};
  }else{
    ostringstream oss;
    oss << str << " @ (" << p << ")" << endl;
    throw Lisp_ptr{zs_new<String>(oss.str())};
  }
}

void throw_zs_error_append(const char* context, Lisp_ptr p){
  ostringstream oss;
  oss << context << " : " << p;
  throw Lisp_ptr{zs_new<String>(oss.str())};
}

void throw_builtin_type_check_failed(Ptr_tag tag, Lisp_ptr p){
  throw_zs_error(p, "arg is not %s!", stringify(tag));
}

void throw_builtin_argcount_failed(const char* name, int required, int max, int passed){
  throw_zs_error({}, "%s: "
                 "number of passed args is mismatched!!"
                 " (acceptable %d-%d args, passed %d)\n",
                 name, required, max, passed);
}

void throw_builtin_identifier_check_failed(Lisp_ptr p){
  throw_zs_error(p, "arg is not identifier!");
}

void throw_builtin_range_check_failed(int max, int passed){
  throw_zs_error({}, "index is out-of-bound ([0, %d), supplied %d\n",
                 max, passed);
}

void throw_number_type_check_failed(Lisp_ptr p){
  throw_zs_error(p, "arg is not number!");
}

void throw_procedure_type_check_failed(Lisp_ptr p){
  throw_zs_error(p, "arg is not procedure!");
}
