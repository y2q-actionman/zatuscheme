#include <cstring>

#include "printer.hh"

using namespace std;

static bool result;

void check(Lisp_ptr input, const char* expect){
  char* buf;
  size_t buf_size;

  FILE* tmp_f = open_memstream(&buf, &buf_size);
  print(tmp_f, input);
  fclose(tmp_f);

  if(strncmp(expect, buf, strlen(expect)) != 0){
    fprintf(stdout, "[failed] expected: %s\n\treturned: %s\n"
            // "bit repl: %x\n"
            , expect, buf
            // , input.base_
            );
    result = false;
  }

  free(buf);
}

int main(){
  result = true;

  check(Lisp_ptr{true}, "#t");
  check(Lisp_ptr{false}, "#f");

  check(Lisp_ptr{'a'}, "#\\a");
  check(Lisp_ptr{'z'}, "#\\z");
  check(Lisp_ptr{'0'}, "#\\0");
  check(Lisp_ptr{' '}, "#\\ ");
  check(Lisp_ptr{'\\'}, "#\\\\");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
