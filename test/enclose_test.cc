#include <cstdlib>
#include <cassert>

#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect,
                                [expect](const char* s){
                                  printf("[failed] expected %s, but got %s\n", expect, s);
                                });
}


int main(){
  install_builtin();

  // testing simple closure
  eval_text("(define x 1)");
  eval_text("(define (hoge) x)");
  check("(hoge)", "1");
  
  eval_text("(define x 100)");
  check("x", "100");
  check("(hoge)", "100");
  

  eval_text("(define fuga (lambda (x) (lambda () x)))");
  eval_text("(define fun123 (fuga 123))");
  check("(fun123)", "123");
  eval_text("(define fun256 (fuga 256))");
  check("(fun256)", "256");

  eval_text("(define x 100)");
  check("x", "100");
  check("(hoge)", "100");
  check("(fun123)", "123");
  check("(fun256)", "256");
       
  eval_text("(define (fun2 x) (+ x 1))");
  check("(fun2 100)", "101");

  // testing defined variables

  // testing various syntaxes

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

