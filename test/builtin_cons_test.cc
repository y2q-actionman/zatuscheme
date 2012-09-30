#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}

void check_undef(const char* input){
  result = !eval_text(input);
}

int main(){
  install_builtin();
  install_builtin_cons();

  check("(pair? '(a . b))", "#t");
  check("(pair? '(a b c))", "#t");
  check("(pair? '())", "#f");
  check("(pair? #(a b))", "#f");

  check("(cons 'a '())", "(a)");
  check("(cons '(a) '(b c d))", "((a) b c d)");
  check("(cons \"a\" '(b c))", "(\"a\" b c)");
  check("(cons 'a 3)", "(a . 3)");
  check("(cons '(a  b) 'c)", "((a b) . c)");
  
  check("(car '(a b c))", "a");
  check("(car '((a) b c d))", "(a)");
  check("(car '(1 . 2))", "1");
  {
    with_null_stream wns;
    check_undef("(car '())");
  }
  
  check("(cdr '((a) b c d))", "(b c d)");
  check("(cdr '(1 . 2))", "2");
  {
    with_null_stream wns;
    check_undef("(cdr '())");
  }

  eval_text("(define tmp (cons 'a 'b))");
  check("tmp", "(a . b)");
  eval_text("(set-car! tmp 1)");
  check("tmp", "(1 . b)");
  eval_text("(set-cdr! tmp 2)");
  check("tmp", "(1 . 2)");

  

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
