#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}

int main(){
  zs_init();

  check("(eqv? 'a 'a)", "#t");
  check("(eqv? 'a 'b)", "#f");
  check("(eqv? 2 2)", "#t");
  check("(eqv? '() '())", "#t");
  check("(eqv? 100000000 100000000)", "#t");
  check("(eqv? (cons 1 2) (cons 1 2))", "#f");
  check("(eqv? (lambda () 1) (lambda () 2))", "#f");
  check("(eqv? #f 'nil)", "#f");
  check("(let ((p (lambda (x) x))) (eqv? p p))", "#t");

  check("(eqv? \"\" \"\")", "#f"); // unspecified
  check("(eqv? '#() '#())", "#f"); // unspecified
  check("(eqv? (lambda (x) x) (lambda (x) x))", "#f"); // unspecified
  check("(eqv? (lambda (x) x) (lambda (y) y))", "#f"); // unspecified

  eval_text("(define gen-counter (lambda () (let ((n 0)) (lambda () (set! n (+ n 1)) n))))");
  check("(let ((g (gen-counter))) (eqv? g g))", "#t");
  check("(eqv? (gen-counter) (gen-counter))", "#f");

  eval_text("(define gen-loser (lambda () (let ((n 0)) (lambda () (set! n (+ n 1)) 27))))");
  check("(let ((g (gen-loser))) (eqv? g g))", "#t");
  check("(eqv? (gen-loser) (gen-loser))", "#f"); // unspecified

  check("(letrec ((f (lambda () (if (eqv? f g) 'both 'f))) (g (lambda () (if (eqv? f g) 'both 'g)))) (eqv? f g))",
        "#f"); // unspecified
  check("(letrec ((f (lambda () (if (eqv? f g) 'f 'both))) (g (lambda () (if (eqv? f g) 'g 'both)))) (eqv? f g))",
        "#f");

  check("(eqv? '(a) '(a))", "#f"); // unspecified
  check("(eqv? \"a\" \"a\")", "#f"); // unspecified
  check("(eqv? '(b) (cdr '(a b)))", "#f"); // unspecified
  check("(let ((x '(a))) (eqv? x x))", "#t");


  check("(eq? 'a 'a)", "#t");
  check("(eq? '(a) '(a))", "#f"); // unspecified
  check("(eq? (list 'a) (list 'a))", "#f");
  check("(eq? \"a\" \"a\")", "#f"); // unspecified
  check("(eq? \"\" \"\")", "#f"); // unspecified
  check("(eq? '() '())", "#t");
  check("(eq? 2 2)", "#f"); // unspecified
  check("(eq? #\\A #\\A)", "#t"); // unspecified
  check("(eq? car car)", "#t");
  check("(let ((n (+ 2 3))) (eq? n n))", "#t"); // unspecified
  check("(let ((x '(a))) (eq? x x))", "#t");
  check("(let ((x '#())) (eq? x x))", "#t");
  check("(let ((p (lambda (x) x))) (eq? p p))", "#t");


  check("(equal? 'a 'a)", "#t");
  check("(equal? '(a) '(a))", "#t");
  check("(equal? '(a (b) c) '(a (b) c))", "#t");
  check("(equal? \"abc\" \"abc\")", "#t");
  check("(equal? 2 2)", "#t");
  check("(equal? (make-vector 5 'a) (make-vector 5 'a))", "#t");
  check("(equal? (lambda (x) x) (lambda (y) y))", "#f"); // unspecified

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
