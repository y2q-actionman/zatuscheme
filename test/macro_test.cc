#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // traditional macro
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


  // syntactic closure
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
  check_e_undef("(eval sc-test-3 (null-environment 5))");

  
  check_e("(identifier? 'a)", "#t");
  check_e("(identifier? (make-syntactic-closure (null-environment 5) '() 'a))",
          "#t");
  check_e("(identifier? \"a\")", "#f");
  check_e("(identifier? #\\a)", "#f");
  check_e("(identifier? 97)", "#f");
  check_e("(identifier? #f)", "#f");
  check_e("(identifier? '(a))", "#f");
  check_e("(identifier? '#(a))", "#f");


  // syntactic closure examples (from MIT-scheme documents)
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
  check_e_success("(push 2 push-test-lis)");
  check_e("push-test-lis", "(2 1)");


  check_e_success(
  "(define-syntax loop"
  " (sc-macro-transformer"
  "  (lambda (exp env)"
  "   (let ((body (cdr exp)))"
  "    `(call-with-current-continuation"
  "      (lambda (exit)"
  "        (let f ()"
  "           ,@(map (lambda (exp)"
  "                     (make-syntactic-closure env '(exit) exp))"
  "                  body)"
  "           (f))))))))");
  check_e_success("loop");
  check_e_success("(define loop-test-cnt 0)");
  check_e("loop-test-cnt", "0");
  with_expect_error([](){
      eval_text("(loop (if (> loop-test-cnt 100) (exit)"
                "        (set! loop-test-cnt (+ loop-test-cnt 1))))");
        });
  check_e("loop-test-cnt", "101");


  check_e_success(
  "(define-syntax push"
  "  (rsc-macro-transformer"
  "   (lambda (exp env)"
  "     `(,(make-syntactic-closure env '() 'set!)"
  "       ,(caddr exp)"
  "       (,(make-syntactic-closure env '() 'cons)"
  "        ,(cadr exp)"
  "        ,(caddr exp))))))");
  check_e_success("(set! push-test-lis ())");
  check_e("push-test-lis", "()");
  check_e_success("(push 1 push-test-lis)");
  check_e("push-test-lis", "(1)");
  check_e_success("(push 2 push-test-lis)");
  check_e("push-test-lis", "(2 1)");

  check_e_success(
  "(define-syntax push"
  "  (sc-macro-transformer"
  "   (lambda (exp usage-env)"
  "     (capture-syntactic-environment"
  "      (lambda (env)"
  "        (make-syntactic-closure usage-env '()"
  "          `(,(make-syntactic-closure env '() 'set!)"
  "            ,(caddr exp)"
  "            (,(make-syntactic-closure env '() 'cons)"
  "             ,(cadr exp)"
  "             ,(caddr exp)))))))))");
  check_e_success("(set! push-test-lis ())");
  check_e("push-test-lis", "()");
  check_e_success("(push 1 push-test-lis)");
  check_e("push-test-lis", "(1)");
  check_e_success("(push 2 push-test-lis)");
  check_e("push-test-lis", "(2 1)");

  
  check_e_success(
  "(define-syntax let1"
  " (sc-macro-transformer"
  "  (lambda (exp env)"
  "   (let ((id (cadr exp))"
  "         (init (caddr exp))"
  "         (exp (cadddr exp)))"
  "    `((lambda (,id)"
  "       ,(make-syntactic-closure env (list id) exp))"
  "      ,(make-syntactic-closure env '() init))))))");
  check_e_success("(define x 1)");
  check_e("x", "1");
  check_e("(let1 x 100 x)", "100");
  check_e("x", "1");
  check_e("(let1 x 100 x)", "100");


  check_e_success(
  "(define-syntax loop-until"
  " (sc-macro-transformer"
  "  (lambda (exp env)"
  "   (let ((id (cadr exp))"
  "         (init (caddr exp))"
  "         (test (cadddr exp))"
  "         (return (cadddr (cdr exp)))"
  "         (step (cadddr (cddr exp)))"
  "         (close"
  "          (lambda (exp free)"
  "           (make-syntactic-closure env free exp))))"
  "       `(letrec ((loop"
  "                  ,(capture-syntactic-environment"
  "                    (lambda (env)"
  "                     `(lambda (,id)"
  "                       (,(make-syntactic-closure env '() `if)"
  "                             ,(close test (list id))"
  "                             ,(close return (list id))"
  "                             (,(make-syntactic-closure env '() `loop)"
  "                        ,(close step (list id)))))))))"
  "       (loop ,(close init '())))))))");
  check_e_success("(define loop-until-test 0)");
  check_e("loop-until-test", "0");
  check_e("(loop-until n 1 (> n 100)"
          "(begin (set! loop-until-test n) 'done) (+ n 1))",
          "done");
  check_e("loop-until-test", "101");
  

  check_e(
  "(let-syntax"
  " ((foo"
  "   (sc-macro-transformer"
  "    (lambda (form env)"
  "     (capture-syntactic-environment"
  "      (lambda (transformer-env)"
  "       (identifier=? transformer-env 'x env 'x)))))))"
  " (list (foo)"
  "  (let ((x 3))"
  "   (foo))))",
  "(#t #f)");
  
  check_e(
  "  (let-syntax ((bar foo))"
  "   (let-syntax"
  "    ((foo"
  "      (sc-macro-transformer"
  "       (lambda (form env)"
  "        (capture-syntactic-environment"
  "         (lambda (transformer-env)"
  "          (identifier=? transformer-env 'foo"
  "                                    env (cadr form))))))))"
  "            (list (foo foo)"
  "                  (foo bar))))",
  "(#f #t)");


  // syntax-rules
  check_e_success(
  "(define-syntax push"
  "  (syntax-rules ()"
  "    ((push item list)"
  "     (set! list (cons item list)))))");
  check_e_success("(set! push-test-lis ())");
  check_e("push-test-lis", "()");
  check_e_success("(push 1 push-test-lis)");
  check_e("push-test-lis", "(1)");
  check_e_success("(push 2 push-test-lis)");
  check_e("push-test-lis", "(2 1)");

  
  // syntax-rules examples from R5RS
  check_e_success(
  "(define-syntax cond"
  "  (syntax-rules (else =>)"
  "    ((cond (else result1 result2 ...))"
  "     (begin result1 result2 ...))"
  "    ((cond (test => result))"
  "     (let ((temp test))"
  "       (if temp (result temp))))"
  "    ((cond (test => result) clause1 clause2 ...)"
  "     (let ((temp test))"
  "       (if temp"
  "           (result temp)"
  "           (cond clause1 clause2 ...))))"
  "    ((cond (test)) test)"
  "    ((cond (test) clause1 clause2 ...)"
  "     (let ((temp test))"
  "       (if temp"
  "           temp"
  "           (cond clause1 clause2 ...))))"
  "    ((cond (test result1 result2 ...))"
  "     (if test (begin result1 result2 ...)))"
  "    ((cond (test result1 result2 ...)"
  "           clause1 clause2 ...)"
  "     (if test"
  "         (begin result1 result2 ...)"
  "         (cond clause1 clause2 ...)))))");
  check_e_success("cond");
  check_e("(cond ((eqv? 1 1) 1))", "1");
  check_e("(cond ((eqv? 1 2) xxx) ((eqv? 2 3) yyy) ((eqv? 3 3) 3))", "3");
  check_e("(cond ((eqv? 1 2) xxx) ((eqv? 2 3) yyy) (else 3))", "3");
  check_e("(cond ((eqv? 1 2)) ((eqv? 2 3) fuga) ((+ 5 7)))", "12");

  check_e_success(
  "(define-syntax case"
  "  (syntax-rules (else)"
  "    ((case (key ...)"
  "       clauses ...)"
  "     (let ((atom-key (key ...)))"
  "       (case atom-key clauses ...)))"
  "    ((case key"
  "       (else result1 result2 ...))"
  "     (begin result1 result2 ...))"
  "    ((case key"
  "       ((atoms ...) result1 result2 ...))"
  "     (if (memv key '(atoms ...))"
  "         (begin result1 result2 ...)))"
  "    ((case key"
  "       ((atoms ...) result1 result2 ...)"
  "       clause clauses ...)"
  "     (if (memv key '(atoms ...))"
  "         (begin result1 result2 ...)"
  "         (case key clause clauses ...)))))");
  check_e_success("case");
  check_e("(case 1 ((1 3 5) 'odd) ((2 4 6) 'even))", "odd");
  check_e("(case a ((1 3 5) 'odd) ((2 4 6) 'even) (else 'wakaran))", "wakaran");

  check_e_success(
  "(define-syntax and"
  "  (syntax-rules ()"
  "    ((and) #t)"
  "    ((and test) test)"
  "    ((and test1 test2 ...)"
  "     (if test1 (and test2 ...) #f))))");
  check_e_success("and");
  check_e("(and)", "#t");
  check_e("(and 1)", "1");
  check_e("(and 1 2)", "2");
  check_e("(and #f 2)", "#f");
  check_e("(and 1 #f 3)", "#f");
  check_e("(and #t #t 3)", "3");

  check_e_success(
  "(define-syntax or"
  "  (syntax-rules ()"
  "    ((or) #f)"
  "    ((or test) test)"
  "    ((or test1 test2 ...)"
  "     (let ((x test1))"
  "       (if x x (or test2 ...))))))");
  check_e_success("or");
  check_e("(or)", "#f");
  check_e("(or 1)", "1");
  check_e("(or #f 2)", "2");
  check_e("(or #f #f 3)", "3");
  check_e("(or 1 #f 3)", "1");
  check_e("(or #f 2 #f 4)", "2");

  check_e_success(
  "(define-syntax let"
  "  (syntax-rules ()"
  "    ((let ((name val) ...) body1 body2 ...)"
  "     ((lambda (name ...) body1 body2 ...)"
  "      val ...))"
  "    ((let tag ((name val) ...) body1 body2 ...)"
  "     ((letrec ((tag (lambda (name ...)"
  "                      body1 body2 ...)))"
  "        tag)"
  "      val ...))))");
  check_e("(let ((x 1) (y 2) (z 3)) x)", "1");
  check_e("(let ((x 1) (y 2) (z 3)) y)", "2");
  check_e("(let ((x 1) (y 2) (z 3)) z)", "3");
  check_e("(let ((x 1)) (let ((x 2)) x))", "2");
  check_e("(let ((x 1)) (let ((x 2)) x) x)", "1");


  return RESULT;
}
