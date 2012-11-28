#ifndef TEST_UTIL_I_HH
#define TEST_UTIL_I_HH

#ifndef TEST_UTIL_HH
#error "Please include via parent file"
#endif

#include "util.hh"
#include "lisp_ptr.hh"
#include "printer.hh"


template<typename Fun>
bool test_on_print(Lisp_ptr input, const char* expect, Fun&& callback){
  std::stringstream ss;
  print(ss, input);

  auto evaled = ss.str();

  if(expect != evaled){
    callback(evaled.c_str());
    return false;
  }

  return true;
}

namespace test_util_detail {

struct with_null_stream{
  FILE* in;
  FILE* out;
  FILE* err;
  decltype(std::cerr.rdbuf()) orig_obuf;

  with_null_stream();
  ~with_null_stream();
};

} // namespace test_util_detail

template<typename Fun>
void with_expect_error(Fun f){
  test_util_detail::with_null_stream wns;
  try{
    f();
  }catch(zs_error&){}
}

#endif // TEST_UTIL_I_HH
