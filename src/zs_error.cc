#include <cstdarg>
#include <cstdio>
#include <exception>
#include <iostream>
#include <sstream>
#include <string>

#include "lisp_ptr.hh"
#include "printer.hh"
#include "rational.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

static const size_t ERROR_MESSAGE_LENGTH = 256;

using namespace std;

void zs_terminate_handler() noexcept{
  std::set_terminate(nullptr);

  try{
    rethrow_exception(current_exception());
  }catch(const Lisp_ptr& errobj){
    cerr << "uncaught exception!\n"
         << "raised object: " << errobj << '\n'
         << endl;
  }catch(const std::exception& e){
    cerr << "uncaught system exception!\n"
         << "what: " << e.what()
         << endl;
  }catch(...){
    cerr << "unexpected internal exception!\n"
         << endl;
  }

  cerr << "Aborting..." << endl;
  abort();
}

static
string vprintf_string(const char* fmt, va_list ap){
  string str(ERROR_MESSAGE_LENGTH, '\0');
  vsnprintf(&(str[0]), str.size(), fmt, ap);
  return str;
}

// error functions
void throw_zs_error(Lisp_ptr p, const char* fmt, ...){
  va_list ap;
  va_start(ap, fmt);
  auto str = vprintf_string(fmt, ap);
  va_end(ap);

  if(!p){
    throw Lisp_ptr{zs_new<String>(move(str))};
  }else{
    ostringstream oss;
    oss << str << " @ (" << p << ")" << endl;
    throw Lisp_ptr{zs_new<String>(oss.str())};
  }
}

void throw_zs_error_append(Lisp_ptr context, Lisp_ptr p){
  ostringstream oss;
  oss << context << " : " << p;
  throw Lisp_ptr{zs_new<String>(oss.str())};
}

void throw_builtin_type_check_failed(Ptr_tag tag, Lisp_ptr p){
  throw_zs_error(p, "arg is not %s!", stringify(tag));
}

void throw_builtin_argcount_failed(Lisp_ptr name, int required, int max, int passed){
  throw_zs_error(name,
                 "number of passed args is mismatched!!"
                 " (acceptable %d-%d args, passed %d)\n",
                 required, max, passed);
}

void throw_builtin_identifier_check_failed(Lisp_ptr p){
  throw_zs_error(p, "arg is not identifier!");
}

void throw_builtin_range_check_failed(size_t max, int passed){
  // The 'z' specifier is a C99 feature, included in C++11.
  throw_zs_error({}, "index is out-of-bound ([0, %zu), supplied %d\n",
                 max, passed);
}

void throw_number_type_check_failed(Lisp_ptr p){
  throw_zs_error(p, "arg is not number!");
}

void throw_procedure_type_check_failed(Lisp_ptr p){
  throw_zs_error(p, "arg is not procedure!");
}

void print_zs_warning(const char* fmt, ...){
  va_list ap;
  va_start(ap, fmt);
  auto str = vprintf_string(fmt, ap);
  va_end(ap);

  cerr << str << endl;
}

void check_type(Ptr_tag tag, Lisp_ptr p){
  if(p.tag() != tag){
    throw_builtin_type_check_failed(tag, p);
  }
}

void check_numeric_type(Lisp_ptr p){
  if(!is_numeric_type(p)){
    throw_number_type_check_failed(p);
  }
}
