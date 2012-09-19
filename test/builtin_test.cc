#include <cstdlib>
#include <cassert>

#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect,
                                [expect](const char* s){
                                  fprintf(zs::err, "[failed] expected %s, but got %s\n", expect, s);
                                });
}


int main(){
  install_builtin();

  check("(list 1)", "(1)");
  check("(list 1 2)", "(1 2)");
  check("(list 1 2 3)", "(1 2 3)");

  check("(list* 1)", "1");
  check("(list* 1 2)", "(1 . 2)");
  check("(list* 1 2 3)", "(1 2 . 3)");

  check("(vector 1)", "#(1)");
  check("(vector 1 2)", "#(1 2)");
  check("(vector 1 2 3)", "#(1 2 3)");

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

  check("(eval 1 ())", "1");
  check("(eval (+ 1 3) ())", "4");
  check("(eval '(+ 1 3) ())", "4");
  check("(eval '(if (eqv? 1 2) \"same\" \"different\") ())", "\"different\"");

  check("(begin (set! tmp-func (lambda (x) `(set! ,x ',x))) #t)", "#t");
  check("(tmp-func 'a)", "(set! a (quote a))");
  check("(begin (set! tmp-macro (to-macro-procedure tmp-func)) #t)", "#t");
  check("(begin (tmp-macro a) #t)", "#t");
  check("a", "a");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

