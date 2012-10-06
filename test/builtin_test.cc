#include <cstdlib>
#include <cassert>

#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}


int main(){
  install_builtin();

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

  check("(cond ((eqv? 1 1) 1))", "1");
  check("(cond ((eqv? 1 2) xxx) ((eqv? 2 3) yyy) ((eqv? 3 3) 3))", "3");
  check("(cond ((eqv? 1 2) xxx) ((eqv? 2 3) yyy) (else 3))", "3");
  check("(cond ((eqv? 1 2)) ((eqv? 2 3) fuga) ((+ 5 7)))", "12");

  check("(case 1 ((1 3 5) 'odd) ((2 4 6) 'even))", "odd");
  check("(case a ((1 3 5) 'odd) ((2 4 6) 'even) (else 'wakaran))", "wakaran");

  check("(begin (set! tmp-func (lambda (x) `(set! ,x ',x))) #t)", "#t");
  check("(tmp-func 'a)", "(set! a (quote a))");
  check("(begin (set! tmp-macro (to-macro-procedure tmp-func)) #t)", "#t");
  check("(begin (tmp-macro a) #t)", "#t");
  check("a", "a");

  check("(eval 1 (null-environment 5))", "1");
  check("(eval (+ 1 3) (scheme-report-environment 5))", "4");
  check("(eval '(+ 1 3) (scheme-report-environment 5))", "4");
  check("(eval '(if (eqv? 1 2) \"same\" \"different\") (interaction-environment))", "\"different\"");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
