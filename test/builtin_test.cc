#include <cstdlib>
#include <cassert>

#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}


int main(){
  zs_init();

  // syntaxes

  check("(and)", "#t");
  check("(and 1)", "1");
  check("(and 1 2)", "2");
  check("(and #f 2)", "#f");
  check("(and 1 #f 3)", "#f");
  check("(and #t #t 3)", "3");

  check("(or)", "#f");
  check("(or 1)", "1");
  check("(or #f 2)", "2");
  check("(or #f #f 3)", "3");
  check("(or 1 #f 3)", "1");
  check("(or #f 2 #f 4)", "2");

  //check("(let () 100)", "100");
  //check("(let (x) x)", "()");
  check("(let ((x 1) (y 2) (z 3)) x)", "1");
  check("(let ((x 1) (y 2) (z 3)) y)", "2");
  check("(let ((x 1) (y 2) (z 3)) z)", "3");
  check("(let ((x 1)) (let ((x 2)) x))", "2");
  check("(let ((x 1)) (let ((x 2)) x) x)", "1");

  // TODO: add more named-let patterns
  // check("(let 100 ((x 0)) x)", "<undef>");
  check("(let loop ((x #f)) (if x x (loop #t)))", "#t");
  // check("(let loop ((x #f)) (if x x (loop #f)))", "#t"); // infinite loop!

  check("(let* ((x 1)) x)", "1");
  check("(let* ((x 1) (y x)) y)", "1");
  check("(let* ((x 1) (y x) (z y)) z)", "1");
  check("(let* ((x 1)) (let ((x 2)) x))", "2");
  check("(let* ((x 1)) (let ((x 2)) x) x)", "1");

  check("(letrec ((x 1)) x)", "1");
  check("(letrec ((x 1) (y 2)) y)", "2");
  check("(letrec ((x 1) (y 2) (z 3)) z)", "3");
  check("(letrec ((x 1)) (let ((x 2)) x))", "2");
  check("(letrec ((x 1)) (let ((x 2)) x) x)", "1");
  check("(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))"
        "         (odd?  (lambda (n) (if (zero? n) #f (even? (- n 1))))))"
        "  (even? 88))",
        "#t");

  check("(cond ((eqv? 1 1) 1))", "1");
  check("(cond ((eqv? 1 2) xxx) ((eqv? 2 3) yyy) ((eqv? 3 3) 3))", "3");
  check("(cond ((eqv? 1 2) xxx) ((eqv? 2 3) yyy) (else 3))", "3");
  check("(cond ((eqv? 1 2)) ((eqv? 2 3) fuga) ((+ 5 7)))", "12");

  check("(case 1 ((1 3 5) 'odd) ((2 4 6) 'even))", "odd");
  check("(case a ((1 3 5) 'odd) ((2 4 6) 'even) (else 'wakaran))", "wakaran");

  check("(begin (define tmp-func (lambda (x) `(define ,x ',x))) #t)", "#t");
  check("(tmp-func 'a)", "(define a (quote a))");
  check("(begin (define tmp-macro (to-macro-procedure tmp-func)) #t)", "#t");
  check("(begin (tmp-macro a) #t)", "#t");
  check("a", "a");

  check("(eval 1 (null-environment 5))", "1");
  check("(eval (+ 1 3) (scheme-report-environment 5))", "4");
  check("(eval '(+ 1 3) (scheme-report-environment 5))", "4");
  check("(eval '(if (eqv? 1 2) \"same\" \"different\") (interaction-environment))", "\"different\"");

  check("(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))",
        "#(0 1 2 3 4)");

  // control funcs

  check("(apply + (list 3 4))", "7");
  check("(apply + (list 3 4) (list 1 2))", "10");
  eval_text("(define compose (lambda (f g) (lambda args (f (apply g args)))))");
  check("((compose sqrt *) 12 75)", "30");

  check("(map cadr '((a b) (d e) (g h)))", "(b e h)");
  check("(map (lambda (n) (expt n n)) '(1 2 3 4 5))", "(1 4 27 256 3125)");
  check("(map + '(1 2 3) '(4 5 6))", "(5 7 9)");
  check("(let ((count 0))"
        "  (map (lambda (ignored)"
        "         (set! count (+ count 1))"
        "         count)"
        "       '(a b)))",
        "(1 2)");

  check("(let ((v (make-vector 5)))"
        "  (for-each (lambda (i) (vector-set! v i (* i i)))"
        "            '(0 1 2 3 4))"
        "  v)",
        "#(0 1 4 9 16)");

  check("(call-with-values (lambda () (values 4 5)) (lambda (a b) b))", "5");
  check("(call-with-values * -)", "-1");

  check("(call-with-current-continuation (lambda (c) (c 1)))", "1");
  eval_text("(define list-length (lambda (obj) (call-with-current-continuation"
            "  (lambda (return) (letrec ((r"
            "    (lambda (obj) (cond ((null? obj) 0) ((pair? obj) (+ (r (cdr obj)) 1)) (else (return #f))))))"
            "  (r obj))))))");
  check("(list-length '(1 2 3 4))", "4");
  check("(list-length '(a b . c))", "#f");


  check("(let ((n 0)) (dynamic-wind"
        "  (lambda () (set! n (+ n 1)))"
        "  (lambda () (set! n (+ n 10)))"
        "  (lambda () (set! n (+ n 100))))"
        "  n)",
        "111");

  check("(let ((path '())"
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

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
