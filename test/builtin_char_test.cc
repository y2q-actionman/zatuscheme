#include "zs.hh"
#include "test_util.hh"

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}

// TODO: if case-insensitivity is supported, define CASE_INSENSITIVE


int main(){
  zs_init();

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


  check("(char-ci=? #\\0 #\\1)", "#f");
  check("(char-ci=? #\\1 #\\1)", "#t");
  check("(char-ci=? #\\2 #\\1)", "#f");

  check("(char-ci<? #\\0 #\\1)", "#t");
  check("(char-ci<? #\\1 #\\1)", "#f");
  check("(char-ci<? #\\2 #\\1)", "#f");

  check("(char-ci>? #\\0 #\\1)", "#f");
  check("(char-ci>? #\\1 #\\1)", "#f");
  check("(char-ci>? #\\2 #\\1)", "#t");

  check("(char-ci<=? #\\0 #\\1)", "#t");
  check("(char-ci<=? #\\1 #\\1)", "#t");
  check("(char-ci<=? #\\2 #\\1)", "#f");

  check("(char-ci>=? #\\0 #\\1)", "#f");
  check("(char-ci>=? #\\1 #\\1)", "#t");
  check("(char-ci>=? #\\2 #\\1)", "#t");

  check("(char-ci=? #\\a #\\A)", "#t");
  check("(char-ci=? #\\0 #\\Z)", "#f");


  check("(char-alphabetic? #\\a)", "#t");
  check("(char-alphabetic? #\\A)", "#t");
  check("(char-alphabetic? #\\0)", "#f");
  check("(char-alphabetic? #\\space)", "#f");

  check("(char-numeric? #\\a)", "#f");
  check("(char-numeric? #\\A)", "#f");
  check("(char-numeric? #\\0)", "#t");
  check("(char-numeric? #\\space)", "#f");

  check("(char-whitespace? #\\a)", "#f");
  check("(char-whitespace? #\\A)", "#f");
  check("(char-whitespace? #\\0)", "#f");
  check("(char-whitespace? #\\space)", "#t");

  check("(char-upper-case? #\\a)", "#f");
  check("(char-upper-case? #\\A)", "#t");
  check("(char-upper-case? #\\0)", "#f");
  check("(char-upper-case? #\\space)", "#f");

  check("(char-lower-case? #\\a)", "#t");
  check("(char-lower-case? #\\A)", "#f");
  check("(char-lower-case? #\\0)", "#f");
  check("(char-lower-case? #\\space)", "#f");

  check("(char=? #\\a (integer->char (char->integer #\\a)))", "#t");
  check("(= 100 (char->integer (integer->char 100)))", "#t");

  check("(char-ci=? #\\a #\\A)", "#t");
  check("(char=? #\\a (char-downcase #\\A))", "#t");
  check("(char=? #\\A (char-upcase #\\a))", "#t");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
