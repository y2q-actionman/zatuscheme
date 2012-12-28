#include <cstdlib>

#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // control funcs
  check_e("(apply + (list 3 4))", "7");
  check_e("(apply + (list 3 4) (list 1 2))", "10");
  eval_text("(define compose (lambda (f g) (lambda args (f (apply g args)))))");
  check_e("((compose sqrt *) 12 75)", "30");

  check_e("(map cadr '((a b) (d e) (g h)))", "(b e h)");
  check_e("(map (lambda (n) (expt n n)) '(1 2 3 4 5))", "(1 4 27 256 3125)");
  check_e("(map + '(1 2 3) '(4 5 6))", "(5 7 9)");
  check_e("(let ((count 0))"
          "  (map (lambda (ignored)"
          "         (set! count (+ count 1))"
          "         count)"
          "       '(a b)))",
          "(1 2)");

  check_e("(let ((v (make-vector 5)))"
          "  (for-each (lambda (i) (vector-set! v i (* i i)))"
          "            '(0 1 2 3 4))"
          "  v)",
          "#(0 1 4 9 16)");

  check_e("(call-with-values (lambda () (values 4 5)) (lambda (a b) b))", "5");
  check_e("(call-with-values * -)", "-1");

  check_e("(call-with-current-continuation (lambda (c) (c 1)))", "1");
  eval_text("(define list-length (lambda (obj) (call-with-current-continuation"
            "  (lambda (return) (letrec ((r"
            "    (lambda (obj) (cond ((null? obj) 0) ((pair? obj) (+ (r (cdr obj)) 1)) (else (return #f))))))"
            "  (r obj))))))");
  check_e("(list-length '(1 2 3 4))", "4");
  check_e("(list-length '(a b . c))", "#f");


  check_e("(let ((n 0)) (dynamic-wind"
          "  (lambda () (set! n (+ n 1)))"
          "  (lambda () (set! n (+ n 10)))"
          "  (lambda () (set! n (+ n 100))))"
          "  n)",
          "111");

  check_e("(let ((path '())"
          "      (c #f))"
          "  (let ((add (lambda (s)"
          "               (set! path (cons s path)))))"
          "    (dynamic-wind"
          "      (lambda () (add 'connect))"
          "      (lambda ()"
          "        (add (call-with-current-continuation"
          "               (lambda (c0)"
          "                 (set! c c0)"
          "                 'talk1))))"
          "      (lambda () (add 'disconnect)))"
          "    (if (< (length path) 4)"
          "        (c 'talk2)"
          "        (reverse path))))",
          "(connect talk1 disconnect connect talk2 disconnect)");

  return (RESULT) ? EXIT_SUCCESS : EXIT_FAILURE;
}
