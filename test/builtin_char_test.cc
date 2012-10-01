#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}

// TODO: if case-insensitivity is supported, define CASE_INSENSITIVE


int main(){
  install_builtin();
  install_builtin_syntax();
  install_builtin_char();

  check("(char? #\\a)", "#t");
  check("(char? 1)", "#f");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

