#include "zs.hh"
#include "test_util.hh"

// TODO: if case-insensitivity is supported, define CASE_INSENSITIVE


int main(){
  zs_init();

  check_e("(char? #\\a)", "#t");
  check_e("(char? 1)", "#f");


  check_e("(char=? #\\0 #\\1)", "#f");
  check_e("(char=? #\\1 #\\1)", "#t");
  check_e("(char=? #\\2 #\\1)", "#f");

  check_e("(char<? #\\0 #\\1)", "#t");
  check_e("(char<? #\\1 #\\1)", "#f");
  check_e("(char<? #\\2 #\\1)", "#f");

  check_e("(char>? #\\0 #\\1)", "#f");
  check_e("(char>? #\\1 #\\1)", "#f");
  check_e("(char>? #\\2 #\\1)", "#t");

  check_e("(char<=? #\\0 #\\1)", "#t");
  check_e("(char<=? #\\1 #\\1)", "#t");
  check_e("(char<=? #\\2 #\\1)", "#f");

  check_e("(char>=? #\\0 #\\1)", "#f");
  check_e("(char>=? #\\1 #\\1)", "#t");
  check_e("(char>=? #\\2 #\\1)", "#t");

  check_e("(char<? #\\a #\\b)", "#t");
  check_e("(char<? #\\A #\\B)", "#t");
  check_e("(char<? #\\0 #\\9)", "#t");
  check_e("(char<? #\\9 #\\a)", "#t");
  check_e("(char<? #\\9 #\\A)", "#t");


  check_e("(char-ci=? #\\0 #\\1)", "#f");
  check_e("(char-ci=? #\\1 #\\1)", "#t");
  check_e("(char-ci=? #\\2 #\\1)", "#f");

  check_e("(char-ci<? #\\0 #\\1)", "#t");
  check_e("(char-ci<? #\\1 #\\1)", "#f");
  check_e("(char-ci<? #\\2 #\\1)", "#f");

  check_e("(char-ci>? #\\0 #\\1)", "#f");
  check_e("(char-ci>? #\\1 #\\1)", "#f");
  check_e("(char-ci>? #\\2 #\\1)", "#t");

  check_e("(char-ci<=? #\\0 #\\1)", "#t");
  check_e("(char-ci<=? #\\1 #\\1)", "#t");
  check_e("(char-ci<=? #\\2 #\\1)", "#f");

  check_e("(char-ci>=? #\\0 #\\1)", "#f");
  check_e("(char-ci>=? #\\1 #\\1)", "#t");
  check_e("(char-ci>=? #\\2 #\\1)", "#t");

  check_e("(char-ci=? #\\a #\\A)", "#t");
  check_e("(char-ci=? #\\0 #\\Z)", "#f");


  check_e("(char-alphabetic? #\\a)", "#t");
  check_e("(char-alphabetic? #\\A)", "#t");
  check_e("(char-alphabetic? #\\0)", "#f");
  check_e("(char-alphabetic? #\\space)", "#f");

  check_e("(char-numeric? #\\a)", "#f");
  check_e("(char-numeric? #\\A)", "#f");
  check_e("(char-numeric? #\\0)", "#t");
  check_e("(char-numeric? #\\space)", "#f");

  check_e("(char-whitespace? #\\a)", "#f");
  check_e("(char-whitespace? #\\A)", "#f");
  check_e("(char-whitespace? #\\0)", "#f");
  check_e("(char-whitespace? #\\space)", "#t");

  check_e("(char-upper-case? #\\a)", "#f");
  check_e("(char-upper-case? #\\A)", "#t");
  check_e("(char-upper-case? #\\0)", "#f");
  check_e("(char-upper-case? #\\space)", "#f");

  check_e("(char-lower-case? #\\a)", "#t");
  check_e("(char-lower-case? #\\A)", "#f");
  check_e("(char-lower-case? #\\0)", "#f");
  check_e("(char-lower-case? #\\space)", "#f");

  check_e("(char=? #\\a (integer->char (char->integer #\\a)))", "#t");
  check_e("(char->integer (integer->char 100))", "100");

  check_e("(char-ci=? #\\a #\\A)", "#t");
  check_e("(char=? #\\a (char-downcase #\\A))", "#t");
  check_e("(char=? #\\A (char-upcase #\\a))", "#t");

  return RESULT;
}
