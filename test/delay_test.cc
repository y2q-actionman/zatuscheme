#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}


int main(){
  install_builtin();

  check("(force (delay (+ 1 2)))", "3");
  check("(let ((p (delay (+ 1 2)))) (list (force p) (force p)))", "(3 3)");

  eval_text("(define a-stream"
            "  (letrec ((next (lambda (n)"
            "      (cons n (delay (next (+ n 1)))))))"
            "    (next 0)))");
  eval_text("(define head car)");
  eval_text("(define tail (lambda (stream) (force (cdr stream))))");
  check("(head (tail (tail a-stream)))", "2");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
