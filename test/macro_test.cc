#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // ==== traditional macro ==== 

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


  // ==== syntactic closure ====

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


  // syntactic closure examples, from comp.lang.scheme
  // https://groups.google.com/forum/?fromgroups=#!topic/comp.lang.scheme/JXXH-_DjwtY
  check_e_success(
  "(define-syntax push!"
  "  (sc-macro-transformer"
  "   (lambda (exp env)"
  "     (let ((item"
  "            (make-syntactic-closure env '() (cadr exp)))"
  "           (list"
  "            (make-syntactic-closure env '() (caddr exp))))"
  "       `(set! ,list (cons ,item ,list))))))");
  eval_text("(define stack '())");
  eval_text("(push! 1 stack)");
  check_e("stack", "(1)");

  check_e_success(
  "(define-syntax swap!"
  "   (sc-macro-transformer"
  "     (lambda (form usage-env)"
  "       (let ((a (make-syntactic-closure usage-env '() (cadr form)))"
  "             (b (make-syntactic-closure usage-env '() (caddr form))))"
  "         `(let ((VALUE ,a))"
  "            (set! ,a ,b)"
  "            (set! ,b VALUE))))))");
  eval_text("(define a 1)");
  eval_text("(define b 2)");
  eval_text("(swap! a b)");
  check_e("a", "2");
  check_e("b", "1");

  check_e(
  "(let ((set! 5))"
  "  (swap! set! b)"
  "  set!)",
  "1");

  check_e_success(
  "(define-syntax aif"
  "   (sc-macro-transformer"
  "     (lambda (form usage-env)"
  "       (let ((condition (make-syntactic-closure usage-env '()"
  "                                                (cadr form)))"
  "             (consequent (make-syntactic-closure usage-env '(it)"
  "                                                 (caddr form)))"
  "             (alternative (make-syntactic-closure usage-env '()"
  "                                                  (cadddr form))))"
  "         `(let ((it ,condition))"
  "            (if it"
  "                ,consequent"
  "                ,alternative))))))");
  check_e(
  "(aif (assv 'a '((1 . 2) (a . b)))"
  "     (cdr it)"
  "     'nothing)",
  "b");
  check_e(
  "(let ((it 1))"
  "  (aif (assv 'a '((1 . 2) (a . b)))"
  "       it"
  "       'nothing))",
  "(a . b)");  // I think binding 'it' should be closed in aif.

  check_e_success(
  "(define-syntax let1"
  "  (sc-macro-transformer"
  "   (lambda (form usage-env)"
  "     (let ((id (cadr form))"
  "           (init (caddr form))"
  "           (exp (cadddr form)))"
  "       `((lambda (,id)"
  "           ,(make-syntactic-closure usage-env (list id)"
  "                                    exp))"
  "         ,(make-syntactic-closure usage-env '()"
  "                                  init))))))");
  check_e("(let1 x 2 (+ x 5))", "7");




  check_e_success(
  "(define-syntax loop"
  "  (sc-macro-transformer"
  "   (lambda (exp env)"
  "     (let ((body (cdr exp)))"
  "       `(call-with-current-continuation"
  "         (lambda (exit)"
  "           (let f ()"
  "             ,@(map (lambda (exp)"
  "                       (make-syntactic-closure env '(exit)"
  "                                               exp))"
  "                     body)"
  "             (f))))))))");

  eval_text("(define loop-result ())");
  eval_text(
  "(let ((x 1))"
  "  (loop (push! x loop-result)"
  "        (if (> x 10)"
  "            (exit x)"
  "            (set! x (+ x 1)))))");
  check_e("loop-result", "(11 10 9 8 7 6 5 4 3 2 1)");

  check_e(
  "(let ((x 1)"
  "      (exit 2))"
  "  (loop (push! x loop-result)"
  "        (if (> x 10)"
  "            (exit x)"
  "            (set! x (+ x 1)))))",
  "11");  // I think binding 'exit' should be closed in aif.


  check_e_success(
  "(define-syntax push"
  "  (sc-macro-transformer"
  "   (lambda (exp usage-env)"
  "     (capture-syntactic-environment"
  "      (lambda (transformer-env)"
  "        (let ((setter"
  "               (make-syntactic-closure transformer-env '() 'set!))"
  "              (item"
  "               (make-syntactic-closure usage-env '() (cadr exp)))"
  "              (list"
  "               (make-syntactic-closure usage-env '() (caddr exp))))"
  "          `(,setter ,list (cons ,item ,list))))))))");
  eval_text("(define stack '())");
  eval_text("(let ((set! #f))"
            "  (push 1 stack))");
  check_e("stack", "(1)");



  // ==== syntax-rules ====

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

  check_e_success(
  "(define-syntax foo"
  "  (syntax-rules ()"
  "    ((_ a ...)"
  "     (list 'a ...))))");
  check_e("(foo 100)", "(100)");
  check_e("(foo 1 2)", "(1 2)");

  
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
  check_e("(case 'some-symbol ((1 3 5) 'odd) ((2 4 6) 'even) (else 'wakaran))", "wakaran");

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

  check_e_success(
  "(define-syntax let*"
  "  (syntax-rules ()"
  "    ((let* () body1 body2 ...)"
  "     (let () body1 body2 ...))"
  "    ((let* ((name1 val1) (name2 val2) ...)"
  "       body1 body2 ...)"
  "     (let ((name1 val1))"
  "       (let* ((name2 val2) ...)"
  "         body1 body2 ...)))))");
  check_e("(let* ((x 1)) x)", "1");
  check_e("(let* ((x 1) (y x)) y)", "1");
  check_e("(let* ((x 1) (y x) (z y)) z)", "1");
  check_e("(let* ((x 1)) (let ((x 2)) x))", "2");
  check_e("(let* ((x 1)) (let ((x 2)) x) x)", "1");

  check_e_success(
  "(define-syntax letrec"
  "  (syntax-rules ()"
  "    ((letrec ((var1 init1) ...) body ...)"
  "     (letrec \"generate_temp_names\""
  "       (var1 ...)"
  "       ()"
  "       ((var1 init1) ...)"
  "       body ...))"
  "    ((letrec \"generate_temp_names\""
  "       ()"
  "       (temp1 ...)"
  "       ((var1 init1) ...)"
  "       body ...)"
  "     (let ((var1 <undefined>) ...)"
  "       (let ((temp1 init1) ...)"
  "         (set! var1 temp1)"
  "         ..."
  "         body ...)))"
  "    ((letrec \"generate_temp_names\""
  "       (x y ...)"
  "       (temp ...)"
  "       ((var1 init1) ...)"
  "       body ...)"
  "     (letrec \"generate_temp_names\""
  "       (y ...)"
  "       (newtemp temp ...)"
  "       ((var1 init1) ...)"
  "       body ...))))");
  check_e("(letrec ((x 1)) x)", "1");
  check_e("(letrec ((x 1) (y 2)) y)", "2");
  check_e("(letrec ((x 1) (y 2) (z 3)) z)", "3");
  check_e("(letrec ((x 1)) (let ((x 2)) x))", "2");
  check_e("(letrec ((x 1)) (let ((x 2)) x) x)", "1");
  check_e("(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))"
          "         (odd?  (lambda (n) (if (zero? n) #f (even? (- n 1))))))"
          "  (even? 88))",
          "#t");

  check_e_success(
  "(define-syntax begin"
  "  (syntax-rules ()"
  "    ((begin exp ...)"
  "     ((lambda () exp ...)))))");
  check_e("(begin 1)", "1");
  check_e("(begin 1 2)", "2");
  check_e("(begin 1 2 3)", "3");

  check_e_success(
  "(define-syntax begin"
  "  (syntax-rules ()"
  "    ((begin exp)"
  "     exp)"
  "    ((begin exp1 exp2 ...)"
  "     (let ((x exp1))"
  "       (begin exp2 ...)))))");
  check_e("(begin 1)", "1");
  check_e("(begin 1 2)", "2");
  check_e("(begin 1 2 3)", "3");

  check_e_success(
  "(define-syntax do"
  "  (syntax-rules ()"
  "    ((do ((var init step ...) ...)"
  "         (test expr ...)"
  "         command ...)"
  "     (letrec"
  "       ((loop"
  "         (lambda (var ...)"
  "           (if test"
  "               (begin"
  "                 <undefined>" // in R5RS, (if #f #f)
  "                 expr ...)"
  "               (begin"
  "                 command"
  "                 ..."
  "                 (loop (do \"step\" var step ...)"
  "                       ...))))))"
  "       (loop init ...)))"
  "    ((do \"step\" x)"
  "     x)"
  "    ((do \"step\" x y)"
  "     y)))");
  check_e("(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))",
          "#(0 1 2 3 4)");


  return RESULT;
}
