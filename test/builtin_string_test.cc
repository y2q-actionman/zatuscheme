#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}

// TODO: if case-insensitivity is supported, define CASE_INSENSITIVE


int main(){
  install_builtin();

  check("(string? \"foo\")", "#t");
  check("(string? 'foo)", "#f");

  check("(make-string 10 #\\a)", "\"aaaaaaaaaa\"");

  check("(string #\\a)", "\"a\"");
  check("(string #\\a #\\b #\\c)", "\"abc\"");
  

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

