#ifndef TEST_UTIL_I_HH
#define TEST_UTIL_I_HH

#ifndef TEST_UTIL_HH
#error "Please include via parent file"
#endif

#include <cstring>
#include <cstdlib>
#include <memory>

#include "lisp_ptr.hh"
#include "printer.hh"


template<typename Fun>
bool test_on_print(Lisp_ptr input, const char* expect, const Fun& callback){
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

  if(strncmp(expect, buf, strlen(expect)) != 0){
    callback(buf);
    goto end;
  }

  ret = true;

 end:
  free(buf);
  return ret;
}

#endif // TEST_UTIL_I_HH
