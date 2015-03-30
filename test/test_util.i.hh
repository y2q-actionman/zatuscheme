#ifndef TEST_UTIL_I_HH
#define TEST_UTIL_I_HH

#ifndef TEST_UTIL_HH
#error "Please include via parent file"
#endif

#include <iostream>
#include "printer.hh"
#include "zs_error.hh"

namespace test_util_detail {

struct with_null_stream{
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
  }catch(zs::Lisp_ptr errobj){
    std::cerr << errobj << std::endl;
  }
}

#endif // TEST_UTIL_I_HH
