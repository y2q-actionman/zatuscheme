#include <cstdlib>

#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // syntaxes
  check_e("(and)", "#t");
  check_e("(and 1)", "1");
  check_e("(and 1 2)", "2");
  check_e("(and #f 2)", "#f");
  check_e("(and 1 #f 3)", "#f");
  check_e("(and #t #t 3)", "3");

  check_e("(or)", "#f");
  check_e("(or 1)", "1");
  check_e("(or #f 2)", "2");
  check_e("(or #f #f 3)", "3");
  check_e("(or 1 #f 3)", "1");
  check_e("(or #f 2 #f 4)", "2");

  //check_e("(let () 100)", "100");
  //check_e("(let (x) x)", "()");
  check_e("(let ((x 1) (y 2) (z 3)) x)", "1");
  check_e("(let ((x 1) (y 2) (z 3)) y)", "2");
  check_e("(let ((x 1) (y 2) (z 3)) z)", "3");
  check_e("(let ((x 1)) (let ((x 2)) x))", "2");
  check_e("(let ((x 1)) (let ((x 2)) x) x)", "1");

  // TODO: add more named-let patterns
  // check_e("(let 100 ((x 0)) x)", "<undef>");
  check_e("(let loop ((x #f)) (if x x (loop #t)))", "#t");
  // check_e("(let loop ((x #f)) (if x x (loop #f)))", "#t"); // infinite loop!

  check_e("(let* ((x 1)) x)", "1");
  check_e("(let* ((x 1) (y x)) y)", "1");
  check_e("(let* ((x 1) (y x) (z y)) z)", "1");
  check_e("(let* ((x 1)) (let ((x 2)) x))", "2");
  check_e("(let* ((x 1)) (let ((x 2)) x) x)", "1");

  check_e("(letrec ((x 1)) x)", "1");
  check_e("(letrec ((x 1) (y 2)) y)", "2");
  check_e("(letrec ((x 1) (y 2) (z 3)) z)", "3");
  check_e("(letrec ((x 1)) (let ((x 2)) x))", "2");
  check_e("(letrec ((x 1)) (let ((x 2)) x) x)", "1");
  check_e("(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))"
          "         (odd?  (lambda (n) (if (zero? n) #f (even? (- n 1))))))"
          "  (even? 88))",
          "#t");

  check_e("(cond ((eqv? 1 1) 1))", "1");
  check_e("(cond ((eqv? 1 2) xxx) ((eqv? 2 3) yyy) ((eqv? 3 3) 3))", "3");
  check_e("(cond ((eqv? 1 2) xxx) ((eqv? 2 3) yyy) (else 3))", "3");
  check_e("(cond ((eqv? 1 2)) ((eqv? 2 3) fuga) ((+ 5 7)))", "12");

  check_e("(case 1 ((1 3 5) 'odd) ((2 4 6) 'even))", "odd");
  check_e("(case a ((1 3 5) 'odd) ((2 4 6) 'even) (else 'wakaran))", "wakaran");

  check_e("(begin (define tmp-func (lambda (x) `(define ,x ',x))) #t)", "#t");
  check_e("(tmp-func 'a)", "(define a (quote a))");
  check_e("(begin (define tmp-macro (to-macro-procedure tmp-func)) #t)", "#t");
  check_e("(begin (tmp-macro a) #t)", "#t");
  check_e("a", "a");

  check_e("(eval 1 (null-environment 5))", "1");
  check_e("(eval (+ 1 3) (scheme-report-environment 5))", "4");
  check_e("(eval '(+ 1 3) (scheme-report-environment 5))", "4");
  check_e("(eval '(if (eqv? 1 2) \"same\" \"different\") (interaction-environment))", "\"different\"");

  check_e("(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))",
          "#(0 1 2 3 4)");

  return (RESULT) ? EXIT_SUCCESS : EXIT_FAILURE;
}
