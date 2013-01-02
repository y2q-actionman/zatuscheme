#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // traditional macro (extension)
  check_e_success("(define tmp-func (lambda (x) `(define ,x ',x)))");
  check_e("(tmp-func 'a)", "(define a (quote a))");

  check_e_success("(define-syntax tmp-macro (traditional-transformer tmp-func))");
  check_e_undef("a");
  check_e_success("(tmp-macro a)");
  check_e("a", "a");

  check_e_success("(define-syntax swap! (traditional-transformer"
                  "(lambda (x y)"
                  "  (let ((tmp (gensym)))"
                  "    `(let ((,tmp '()))"
                  "       (set! ,tmp ,x) (set! ,x ,y) (set! ,y ,tmp))))))");
  check_e_success("(define x 1)");
  check_e_success("(define y 2)");
  check_e("x", "1");
  check_e("y", "2");
  check_e_success("(swap! x y)");
  check_e("x", "2");
  check_e("y", "1");

  check_e_success("(define-syntax sc-test-1 (sc-macro-transformer"
                  "(lambda (form env) (cadr form))))");
  check_e("(sc-test-1 (list x y))", "(2 1)");
  check_e_success("(swap! x y)");
  check_e("x", "1");
  check_e("y", "2");
  check_e("(sc-test-1 (list x y))", "(2 1)");

  check_e_success("(define-syntax sc-test-2 (sc-macro-transformer"
                  "(lambda (form env)"
                  "  (make-syntactic-closure env () (cadr form)))))");
  check_e("(sc-test-2 (list x y))", "(1 2)");
  check_e_success("(swap! x y)");
  check_e("x", "2");
  check_e("y", "1");
  check_e("(sc-test-2 (list x y))", "(2 1)");

  check_e_success("(define sc-test-3"
                  "  (make-syntactic-closure (interaction-environment) '(x) 'x))");
  check_e_success("sc-test-3");
  check_e("(eval sc-test-3 (null-environment 5))", "x");


  check_e_success(
  "(define-syntax push"
  "  (sc-macro-transformer"
  "   (lambda (exp env)"
  "    (let ((item (make-syntactic-closure env '() (cadr exp)))"
  "          (list (make-syntactic-closure env '() (caddr exp))))"
  "     `(set! ,list (cons ,item ,list))))))");
  check_e_success("(define push-test-lis ())");
  check_e("push-test-lis", "()");
  check_e_success("(push 1 push-test-lis)");
  check_e("push-test-lis", "(1)");

  return RESULT;
}
