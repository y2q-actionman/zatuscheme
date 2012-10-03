#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}

void check_undef(const char* input){
  result = !eval_text(input);
}

void check_success(const char* input){
  result = !!eval_text(input);
}

// TODO: if case-insensitivity is supported, define CASE_INSENSITIVE


int main(){
  install_builtin();

  check("(string? \"foo\")", "#t");
  check("(string? 'foo)", "#f");

  check("(make-string 10 #\\a)", "\"aaaaaaaaaa\"");

  check("(string #\\a)", "\"a\"");
  check("(string #\\a #\\b #\\c)", "\"abc\"");
  
  check("(string-length \"\")", "0");
  check("(string-length \"a\")", "1");
  check("(string-length \"abcde\")", "5");

  check("(string-ref \"a\" 0)", "#\\a");
  check("(string-ref \"abcde\" 3)", "#\\d");
  {
    with_null_stream wns;
    check_undef("(string-ref \"a\" -1)");
    check_undef("(string-ref \"a\" 100)");
    check_undef("(string-ref \"\" 0)");
  }

  check("(define tmpstr (make-string 3 #\\*))", "\"***\"");
  eval_text("(string-set! tmpstr 0 #\\?)");
  check("tmpstr", "\"?**\"");
  eval_text("(string-set! tmpstr 1 #\\!)");
  check("tmpstr", "\"?!*\"");
  {
    with_null_stream wns;
    check_undef("(string-set! tmpstr -1 #\\_)");
    check_undef("(string-set! tmpstr 100 #\\_)");
  }

  // when immutable string implemented..
  // eval_text("(define (f) (make-string 3 #\\*))");
  // eval_text("(define (g) \"***\")");
  // check_success("(string-set! (f) 0 #\\?)");
  // check_undef("(string-set! (g) 0 #\\?)");
  // check_undef("(string-set! (symbol->string 'immutable) 0 #\\?)");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

