#ifndef TEST_UTIL_I_HH
#define TEST_UTIL_I_HH

#ifndef TEST_UTIL_HH
#error "Please include via parent file"
#endif

#include <cstring>
#include <cstdlib>
#include <memory>

#include "util.hh"
#include "lisp_ptr.hh"
#include "printer.hh"


template<typename Fun>
bool test_on_print(Lisp_ptr input, const char* expect, Fun&& callback){
  bool ret = false;
  char* buf = NULL;
  size_t buf_size = 0;

  {
    FILE* tmp_f = open_memstream(&buf, &buf_size);
    if(!tmp_f){
      perror(__func__);
      goto end;
    }

    print(tmp_f, input);

    if(fclose(tmp_f) != 0){
      perror(__func__);
      goto end;
    }
  }

  if(strcmp(expect, buf) != 0){
    callback(buf);
    goto end;
  }

  ret = true;

 end:
  free(buf);
  return ret;
}

struct with_null_stream{
  FILE* in;
  FILE* out;
  FILE* err;

  with_null_stream();
  ~with_null_stream();
};

template<typename Fun>
void with_expect_error(Fun f){
  with_null_stream wns;
  try{
    f();
  }catch(zs_error&){}
}

#endif // TEST_UTIL_I_HH
