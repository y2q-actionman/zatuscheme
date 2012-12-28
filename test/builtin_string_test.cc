#include "zs.hh"
#include "test_util.hh"

// TODO: if case-insensitivity is supported, define CASE_INSENSITIVE


int main(){
  zs_init();

  check_e("(string? \"foo\")", "#t");
  check_e("(string? 'foo)", "#f");

  check_e("(make-string 10 #\\a)", "\"aaaaaaaaaa\"");

  check_e("(string #\\a)", "\"a\"");
  check_e("(string #\\a #\\b #\\c)", "\"abc\"");
  
  check_e("(string-length \"\")", "0");
  check_e("(string-length \"a\")", "1");
  check_e("(string-length \"abcde\")", "5");

  check_e("(string-ref \"a\" 0)", "#\\a");
  check_e("(string-ref \"abcde\" 3)", "#\\d");
  check_e_undef("(string-ref \"a\" -1)");
  check_e_undef("(string-ref \"a\" 100)");
  check_e_undef("(string-ref \"\" 0)");

  eval_text("(define tmpstr (make-string 3 #\\*))");
  eval_text("(string-set! tmpstr 0 #\\?)");
  check_e("tmpstr", "\"?**\"");
  eval_text("(string-set! tmpstr 1 #\\!)");
  check_e("tmpstr", "\"?!*\"");
  check_e_undef("(string-set! tmpstr -1 #\\_)");
  check_e_undef("(string-set! tmpstr 100 #\\_)");

  // when immutable string implemented..
  // eval_text("(define (f) (make-string 3 #\\*))");
  // eval_text("(define (g) \"***\")");
  // check_e_success("(string-set! (f) 0 #\\?)");
  // check_e_undef("(string-set! (g) 0 #\\?)");
  // check_e_undef("(string-set! (symbol->string 'immutable) 0 #\\?)");


  check_e("(string=? \"aaa\" \"aaa\")", "#t");
  check_e("(string=? \"aaa\" \"aab\")", "#f");
  check_e("(string=? \"aba\" \"aab\")", "#f");

  check_e("(string<? \"aaa\" \"aaa\")", "#f");
  check_e("(string<? \"aaa\" \"aab\")", "#t");
  check_e("(string<? \"aba\" \"aab\")", "#f");

  check_e("(string>? \"aaa\" \"aaa\")", "#f");
  check_e("(string>? \"aaa\" \"aab\")", "#f");
  check_e("(string>? \"aba\" \"aab\")", "#t");

  check_e("(string<=? \"aaa\" \"aaa\")", "#t");
  check_e("(string<=? \"aaa\" \"aab\")", "#t");
  check_e("(string<=? \"aba\" \"aab\")", "#f");

  check_e("(string>=? \"aaa\" \"aaa\")", "#t");
  check_e("(string>=? \"aaa\" \"aab\")", "#f");
  check_e("(string>=? \"aba\" \"aab\")", "#t");

  check_e("(string-ci=? \"Aaa\" \"aaa\")", "#t");
  check_e("(string-ci=? \"Aaa\" \"aab\")", "#f");
  check_e("(string-ci=? \"Aba\" \"aab\")", "#f");

  check_e("(string-ci<? \"Aaa\" \"aaa\")", "#f");
  check_e("(string-ci<? \"Aaa\" \"aab\")", "#t");
  check_e("(string-ci<? \"Aba\" \"aab\")", "#f");

  check_e("(string-ci>? \"Aaa\" \"aaa\")", "#f");
  check_e("(string-ci>? \"Aaa\" \"aab\")", "#f");
  check_e("(string-ci>? \"Aba\" \"aab\")", "#t");

  check_e("(string-ci<=? \"Aaa\" \"aaa\")", "#t");
  check_e("(string-ci<=? \"Aaa\" \"aab\")", "#t");
  check_e("(string-ci<=? \"Aba\" \"aab\")", "#f");

  check_e("(string-ci>=? \"Aaa\" \"aaa\")", "#t");
  check_e("(string-ci>=? \"Aaa\" \"aab\")", "#f");
  check_e("(string-ci>=? \"Aba\" \"aab\")", "#t");


  check_e("(substring \"0123456789\" 1 5)", "\"1234\"");
  check_e("(substring \"0123456789\" 1 1)", "\"\"");
  check_e("(substring \"0123456789\" 0 1)", "\"0\"");
  check_e_undef("(substring \"0123456789\" -1 1)");
  check_e_undef("(substring \"0123456789\" 0 999)");
  check_e_undef("(substring \"0123456789\" 5 4)");
  check_e_undef("(substring \"0123456789\" 199 -1)");

  check_e("(string-append)", "\"\"");
  check_e("(string-append \"\")", "\"\"");
  check_e("(string-append \"\" \"1\" \"\" \"2\" \"3\")", "\"123\"");

  check_e("(string->list \"\")", "()");
  check_e("(string->list \"a\")", "(#\\a)");
  check_e("(string->list \"abc\")", "(#\\a #\\b #\\c)");

  check_e("(list->string '())", "\"\"");
  check_e("(list->string '(#\\a))", "\"a\"");
  check_e("(list->string '(#\\a #\\b #\\c))", "\"abc\"");


  check_e("(string-copy \"abc\")", "\"abc\"");

  eval_text("(define tmpstr (string #\\a #\\b #\\c))");
  eval_text("(string-fill! tmpstr #\\?)");
  check_e("tmpstr", "\"???\"");
  eval_text("(string-fill! tmpstr #\\!)");
  check_e("tmpstr", "\"!!!\"");

  return RESULT;
}
