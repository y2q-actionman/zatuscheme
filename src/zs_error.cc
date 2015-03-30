#include <cstdarg>
#include <cstdio>
#include <exception>
#include <iostream>
#include <sstream>
#include <string>

#include "cons_util.hh"
#include "lisp_ptr.hh"
#include "printer.hh"
#include "procedure.hh"
#include "rational.hh"
#include "s_closure.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

static const size_t ERROR_MESSAGE_LENGTH = 256;

using namespace std;

namespace zs {

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

// error functions
void throw_zs_error(Lisp_ptr p, const char* fmt, ...){
  char strbuf[ERROR_MESSAGE_LENGTH];

  va_list ap;
  va_start(ap, fmt);
  vsnprintf(strbuf, sizeof(strbuf), fmt, ap);
  va_end(ap);

  if(!p){
    throw Lisp_ptr{zs_new<String>(strbuf)};
  }else{
    ostringstream oss;
    oss << strbuf << " @ (" << p << ")" << endl;
    throw Lisp_ptr{zs_new<String>(oss.str())};
  }
}

void throw_zs_error_append(Lisp_ptr context, Lisp_ptr p){
  ostringstream oss;
  oss << context << " : " << p;
  throw Lisp_ptr{zs_new<String>(oss.str())};
}

void print_zs_warning(const char* fmt, ...){
  char strbuf[ERROR_MESSAGE_LENGTH];

  va_list ap;
  va_start(ap, fmt);
  vsnprintf(strbuf, sizeof(strbuf), fmt, ap);
  va_end(ap);

  cerr << strbuf << endl;
}

void check_type(Ptr_tag tag, Lisp_ptr p){
  if(p.tag() != tag){
    throw_zs_error(p, "arg is not %s!", stringify(tag));
  }
}

void check_numeric_type(Lisp_ptr p){
  if(!is_numeric_type(p)){
    throw_zs_error(p, "arg is not number!");
  }
}

void check_identifier_type(Lisp_ptr p){
  if(!identifierp(p)){
    throw_zs_error(p, "arg is not identifier!");
  }
}

void check_procedure_type(Lisp_ptr p){
  if(!is_procedure(p)){
    throw_zs_error(p, "arg is not procedure!");
  }
}

void check_nonnull_cons(Lisp_ptr p){
  if(!is_nonnull_cons(p)){
    throw_zs_error(p, "arg is not cons or null list!");
  }
}

void check_range(Lisp_ptr p, size_t min, size_t max){
  check_type(Ptr_tag::integer, p);
  auto idx = p.get<int>();

  if(idx < static_cast<signed>(min) || idx >= static_cast<signed>(max)){
    // The 'z' specifier is a C99 feature, included in C++11.
    throw_zs_error({}, "inacceptable index ([%zd, %zd), supplied %d\n",
                   min, max, idx);
  }
}

} // namespace zs
