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

  check("(char=? #\\0 #\\1)", "#f");
  check("(char=? #\\1 #\\1)", "#t");
  check("(char=? #\\2 #\\1)", "#f");

  check("(char<? #\\0 #\\1)", "#t");
  check("(char<? #\\1 #\\1)", "#f");
  check("(char<? #\\2 #\\1)", "#f");

  check("(char>? #\\0 #\\1)", "#f");
  check("(char>? #\\1 #\\1)", "#f");
  check("(char>? #\\2 #\\1)", "#t");

  check("(char<=? #\\0 #\\1)", "#t");
  check("(char<=? #\\1 #\\1)", "#t");
  check("(char<=? #\\2 #\\1)", "#f");

  check("(char>=? #\\0 #\\1)", "#f");
  check("(char>=? #\\1 #\\1)", "#t");
  check("(char>=? #\\2 #\\1)", "#t");

  check("(char<? #\\a #\\b)", "#t");
  check("(char<? #\\A #\\B)", "#t");
  check("(char<? #\\0 #\\9)", "#t");
  check("(char<? #\\9 #\\a)", "#t");
  check("(char<? #\\9 #\\A)", "#t");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

