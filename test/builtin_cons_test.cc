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
  install_builtin_numeric();

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

  
  check("(list? '(a b c))", "#t");
  check("(list? '())", "#t");
  check("(list? '(a . b))", "#f");
  check("(let ((x (list 'a))) (set-cdr! x x) (list? x))", "#f");


  check("(list 1)", "(1)");
  check("(list 1 2)", "(1 2)");
  check("(list 1 2 3)", "(1 2 3)");
  check("(list 'a (+ 4 3) 'c)", "(a 7 c)");
  check("(list)", "()");

  check("(list* 1)", "1");
  check("(list* 1 2)", "(1 . 2)");
  check("(list* 1 2 3)", "(1 2 . 3)");


  check("(length '(a b c))", "3");
  check("(length '(a (b) (c d e)))", "3");
  check("(length '())", "0");

  check("(append '(1 2 3))", "(1 2 3)");
  check("(append '(1 2 3) '(4 5 6))", "(1 2 3 4 5 6)");
  check("(append '(1 2 3) '())", "(1 2 3)");
  check("(append '() '(1 2 3))", "(1 2 3)");
  check("(append '(1 2 3) 'a)", "(1 2 3 . a)");
  check("(append '(x) '(y))", "(x y)");
  check("(append '(a) '(b c d))", "(a b c d)");
  check("(append '(a (b)) '((c)))", "(a (b) (c))");
  check("(append '(a b) '(c . d))", "(a b c . d)");
  check("(append '() 'a)", "a");
  check("(append '() '() '() '() 'a)", "a");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
