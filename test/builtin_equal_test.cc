#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  check_e("(eqv? 'a 'a)", "#t");
  check_e("(eqv? 'a 'b)", "#f");
  check_e("(eqv? 2 2)", "#t");
  check_e("(eqv? '() '())", "#t");
  check_e("(eqv? 100000000 100000000)", "#t");
  check_e("(eqv? (cons 1 2) (cons 1 2))", "#f");
  check_e("(eqv? (lambda () 1) (lambda () 2))", "#f");
  check_e("(eqv? #f 'nil)", "#f");
  check_e("(let ((p (lambda (x) x))) (eqv? p p))", "#t");

  check_e("(eqv? \"\" \"\")", "#f"); // unspecified
  check_e("(eqv? '#() '#())", "#f"); // unspecified
  check_e("(eqv? (lambda (x) x) (lambda (x) x))", "#f"); // unspecified
  check_e("(eqv? (lambda (x) x) (lambda (y) y))", "#f"); // unspecified

  eval_text("(define gen-counter (lambda () (let ((n 0)) (lambda () (set! n (+ n 1)) n))))");
  check_e("(let ((g (gen-counter))) (eqv? g g))", "#t");
  check_e("(eqv? (gen-counter) (gen-counter))", "#f");

  eval_text("(define gen-loser (lambda () (let ((n 0)) (lambda () (set! n (+ n 1)) 27))))");
  check_e("(let ((g (gen-loser))) (eqv? g g))", "#t");
  check_e("(eqv? (gen-loser) (gen-loser))", "#f"); // unspecified

  check_e("(letrec ((f (lambda () (if (eqv? f g) 'both 'f))) (g (lambda () (if (eqv? f g) 'both 'g)))) (eqv? f g))",
        "#f"); // unspecified
  check_e("(letrec ((f (lambda () (if (eqv? f g) 'f 'both))) (g (lambda () (if (eqv? f g) 'g 'both)))) (eqv? f g))",
        "#f");

  check_e("(eqv? '(a) '(a))", "#f"); // unspecified
  check_e("(eqv? \"a\" \"a\")", "#f"); // unspecified
  check_e("(eqv? '(b) (cdr '(a b)))", "#f"); // unspecified
  check_e("(let ((x '(a))) (eqv? x x))", "#t");


  check_e("(eq? 'a 'a)", "#t");
  check_e("(eq? '(a) '(a))", "#f"); // unspecified
  check_e("(eq? (list 'a) (list 'a))", "#f");
  check_e("(eq? \"a\" \"a\")", "#f"); // unspecified
  check_e("(eq? \"\" \"\")", "#f"); // unspecified
  check_e("(eq? '() '())", "#t");
  check_e("(eq? 2 2)", "#f"); // unspecified
  check_e("(eq? #\\A #\\A)", "#t"); // unspecified
  check_e("(eq? car car)", "#t");
  check_e("(let ((n (+ 2 3))) (eq? n n))", "#t"); // unspecified
  check_e("(let ((x '(a))) (eq? x x))", "#t");
  check_e("(let ((x '#())) (eq? x x))", "#t");
  check_e("(let ((p (lambda (x) x))) (eq? p p))", "#t");


  check_e("(equal? 'a 'a)", "#t");
  check_e("(equal? '(a) '(a))", "#t");
  check_e("(equal? '(a (b) c) '(a (b) c))", "#t");
  check_e("(equal? \"abc\" \"abc\")", "#t");
  check_e("(equal? 2 2)", "#t");
  check_e("(equal? (make-vector 5 'a) (make-vector 5 'a))", "#t");
  check_e("(equal? (lambda (x) x) (lambda (y) y))", "#f"); // unspecified

  return (RESULT) ? EXIT_SUCCESS : EXIT_FAILURE;
}
