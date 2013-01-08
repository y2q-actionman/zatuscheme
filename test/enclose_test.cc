#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // testing simple closure
  eval_text("(define x 1)");
  eval_text("(define (hoge) x)");
  check_e("(hoge)", "1");
  
  eval_text("(define x 100)");
  check_e("x", "100");
  check_e("(hoge)", "100");
  

  eval_text("(define fuga (lambda (x) (lambda () x)))");
  eval_text("(define fun123 (fuga 123))");
  check_e("(fun123)", "123");
  eval_text("(define fun256 (fuga 256))");
  check_e("(fun256)", "256");

  eval_text("(define x 100)");
  check_e("x", "100");
  check_e("(hoge)", "100");
  check_e("(fun123)", "123");
  check_e("(fun256)", "256");
       
  eval_text("(define (fun2 x) (+ x 1))");
  check_e("(fun2 100)", "101");

  // testing defined variables
  eval_text("(define (hoge) (define (fuga n) (* n 2)) fuga)");
  check_e("((hoge) 100)", "200");

  // testing various syntaxes

  return RESULT;
}
