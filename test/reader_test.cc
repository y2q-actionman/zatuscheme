#include <cstring>
#include <string>

#include "decl.hh"
#include "printer.hh"
#include "reader.hh"
#include "symtable.hh"

using namespace std;

static bool result = true;

void check(SymTable& st, const char* input, const char* expect){
  FILE* in_f = fmemopen((void*)input, strlen(input), "r");
  char* buf;
  size_t buf_size;
  FILE* out_f = open_memstream(&buf, &buf_size);

  Lisp_ptr p{read(st, in_f)};

  fclose(in_f);
  print(out_f, p);
  fclose(out_f);

  if(strncmp(expect, buf, strlen(expect)) != 0){
    fprintf(stdout, "[failed] input:%s, expected: %s\n\treturned: %s\n",
            input, expect, buf);
    result = false;
  }

  free(buf);
}

void check_undef(SymTable& st, const char* input){
  FILE* in_f = fmemopen((void*)input, strlen(input), "r");
  Lisp_ptr p{read(st, in_f)};
  fclose(in_f);

  if(p){
    fprintf(stdout, "[failed] input:%s, expected: (undefined)\n", input);
    result = false;
  }
}

int main(){
  SymTable st;

  // boolean
  check(st, "#t", "#t");
  check(st, "#f", "#f");
  check(st, "   #f", "#f");
  check(st, "#t #f", "#t");

  // char
  check(st, "#\\v", "#\\v");
  check(st, "#\\s", "#\\s");
  check(st, "#\\space", "#\\ ");

  // symbol
  check(st, "a", "a");
  check(st, "+", "+");
  check(st, "*hoge-hoge*", "*hoge-hoge*");

  // number
  check(st, "100", "100");
  check(st, "1.01", "1.01");

  // string
  check(st, "\"\"", "\"\"");
  check(st, "\"aaa aaa\"", "\"aaa aaa\"");
  check(st, "\"aa\\\\a a\\\"aa\"", "\"aa\\a a\"aa\"");
  check_undef(st, "\" \\ \"");

  // cons, list
  check(st, "(#t #f)", "(#t #f)");
  check(st, "(a b c d e)", "(a b c d e)");
  check(st, "(a (b . c) d e)", "(a (b . c) d e)");

  // vector

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
