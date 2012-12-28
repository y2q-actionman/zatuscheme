#ifndef TEST_UTIL_I_HH
#define TEST_UTIL_I_HH

#ifndef TEST_UTIL_HH
#error "Please include via parent file"
#endif

#include <iostream>
#include "util.hh"

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
  }catch(zs_error&){}
}

#endif // TEST_UTIL_I_HH
