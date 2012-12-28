#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  check_e("(force (delay (+ 1 2)))", "3");
  check_e("(let ((p (delay (+ 1 2)))) (list (force p) (force p)))", "(3 3)");

  eval_text("(define a-stream"
            "  (letrec ((next (lambda (n)"
            "      (cons n (delay (next (+ n 1)))))))"
            "    (next 0)))");
  eval_text("(define head car)");
  eval_text("(define tail (lambda (stream) (force (cdr stream))))");
  check_e("(head (tail (tail a-stream)))", "2");

  eval_text("(define count 0)");
  eval_text("(define p (delay (begin (set! count (+ count 1))"
            "                        (if (> count x) count (force p)))))");
  eval_text("(define x 5)");
  check_e("(force p)", "6");
  check_e("(begin (set! x 10) (force p))", "6");

  return (RESULT) ? EXIT_SUCCESS : EXIT_FAILURE;
}
