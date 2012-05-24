#include <cstdlib>
#include <cassert>

#include "eval.hh"
#include "test_util.hh"
#include "builtin.hh"
#include "printer.hh"

static bool result = true;

Lisp_ptr eval_text(const char* s){
  auto exp = read_from_string(s);
  if(!exp){
    printf("[failed] read error on %s\n", s);
    result = false;
    return {};
  }

  auto ret = eval(exp);
  if(!ret){
    printf("[failed] eval error on %s\n", s);
    result = false;
    return {};
  }

  return ret;
}

void check(const char* input, const char* expect){
  auto e = eval_text(input);
  if(!e){
    result = false;
    return;
  }

  if(!test_on_print(e, expect,
                    [expect](const char* s){
                      printf("[failed] expected %s, but got %s\n", expect, s);
                    })){
    result = false;
    return;
  }
}


int main(){
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
       

  // testing defined variables

  // testing various syntaxes

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

