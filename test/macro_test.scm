(load "zs_test_util.scm")

;; ==== traditional macro ==== 
(define tmp-func
  (lambda (x) `(define ,x ',x)))
(test-equal '(define a (quote a)) (tmp-func 'a))

(define-syntax tmp-macro
  (traditional-transformer tmp-func))
;; (test-error a)
(tmp-macro a)
(test-equal 'a a)

(define-syntax swap!
  (traditional-transformer
   (lambda (x y)
     (let ((tmp (gensym)))
       `(let ((,tmp '()))
          (set! ,tmp ,x) (set! ,x ,y) (set! ,y ,tmp))))))

(define x 1)
(define y 2)
(test-eqv 1 x)
(test-eqv 2 y)
(swap! x y)
(test-eqv 2 x)
(test-eqv 1 y)


;; ==== syntactic closure ====
(define-syntax sc-test-1
  (sc-macro-transformer
   (lambda (form env) (cadr form))))
(test-equal '(2 1) (sc-test-1 (list x y)))
(swap! x y)
(test-eqv 1 x)
(test-eqv 2 y)
(test-equal '(2 1) (sc-test-1 (list x y)))

(define-syntax sc-test-2
  (sc-macro-transformer
   (lambda (form env)
     (make-syntactic-closure env () (cadr form)))))
(test-equal '(1 2) (sc-test-2 (list x y)))
(swap! x y)
(test-eqv 2 x)
(test-eqv 1 y)
(test-equal '(2 1) (sc-test-2 (list x y)))

(define sc-test-3
  (make-syntactic-closure (interaction-environment) '(x) 'x))
(test-assert sc-test-3)
;; (test-error (eval sc-test-3 (null-environment 5)))

(test-eq #t (identifier? 'a))
(test-eq #t (identifier? (make-syntactic-closure (null-environment 5) '() 'a)))
(test-eq #f (identifier? "a"))
(test-eq #f (identifier? #\a))
(test-eq #f (identifier? 97))
(test-eq #f (identifier? #f))
(test-eq #f (identifier? '(a)))
(test-eq #f (identifier? '#(a)))

;; syntactic closure examples (from MIT-scheme documents)
(define-syntax push
  (sc-macro-transformer
   (lambda (exp env)
    (let ((item (make-syntactic-closure env '() (cadr exp)))
          (list (make-syntactic-closure env '() (caddr exp))))
      `(set! ,list (cons ,item ,list))))))
(define push-test-lis ())
(test-equal '() push-test-lis)
(push 1 push-test-lis)
(test-equal '(1) push-test-lis)
(push 2 push-test-lis)
(test-equal '(2 1) push-test-lis)

(define-syntax loop
  (sc-macro-transformer
   (lambda (exp env)
     (let ((body (cdr exp)))
       `(call-with-current-continuation
         (lambda (exit)
           (let f ()
             ,@(map (lambda (exp)
                      (make-syntactic-closure env '(exit) exp))
                    body)
             (f))))))))
(test-assert loop)
(define loop-test-cnt 0)
(test-eqv 0 loop-test-cnt)
(loop (if (> loop-test-cnt 100) (exit)
          (set! loop-test-cnt (+ loop-test-cnt 1))))
(test-eqv 101 loop-test-cnt)

(define-syntax push
  (rsc-macro-transformer
   (lambda (exp env)
     `(,(make-syntactic-closure env '() 'set!)
       ,(caddr exp)
       (,(make-syntactic-closure env '() 'cons)
        ,(cadr exp)
        ,(caddr exp))))))
(set! push-test-lis ())
(test-equal '() push-test-lis)
(push 1 push-test-lis)
(test-equal '(1) push-test-lis)
(push 2 push-test-lis)
(test-equal '(2 1) push-test-lis)

(define-syntax push
  (sc-macro-transformer
   (lambda (exp usage-env)
     (capture-syntactic-environment
      (lambda (env)
        (make-syntactic-closure usage-env '()
          `(,(make-syntactic-closure env '() 'set!)
            ,(caddr exp)
            (,(make-syntactic-closure env '() 'cons)
             ,(cadr exp)
             ,(caddr exp)))))))))
(set! push-test-lis ())
(test-equal '() push-test-lis)
(push 1 push-test-lis)
(test-equal '(1) push-test-lis)
(push 2 push-test-lis)
(test-equal '(2 1) push-test-lis)

(define-syntax let1
  (sc-macro-transformer
   (lambda (exp env)
     (let ((id (cadr exp))
           (init (caddr exp))
           (exp (cadddr exp)))
       `((lambda (,id)
           ,(make-syntactic-closure env (list id) exp))
         ,(make-syntactic-closure env '() init))))))
(define x 1)
(test-eqv 1 x)
(test-eqv 100 (let1 x 100 x))
(test-eqv 1 x)
(test-eqv 100 (let1 x 100 x))

(define-syntax loop-until
 (sc-macro-transformer
  (lambda (exp env)
   (let ((id (cadr exp))
         (init (caddr exp))
         (test (cadddr exp))
         (return (cadddr (cdr exp)))
         (step (cadddr (cddr exp)))
         (close
          (lambda (exp free)
           (make-syntactic-closure env free exp))))
       `(letrec ((loop
                  ,(capture-syntactic-environment
                    (lambda (env)
                     `(lambda (,id)
                       (,(make-syntactic-closure env '() `if)
                             ,(close test (list id))
                             ,(close return (list id))
                             (,(make-syntactic-closure env '() `loop)
                        ,(close step (list id)))))))))
       (loop ,(close init '())))))))
(define loop-until-test 0)
(test-eqv 0 loop-until-test)
(test-eqv 'done
          (loop-until n 1 (> n 100)
                      (begin (set! loop-until-test n) 'done) (+ n 1)))
(test-eqv 101 loop-until-test)

(test-equal '(#t #f)
            (let-syntax
                ((foo
                  (sc-macro-transformer
                   (lambda (form env)
                     (capture-syntactic-environment
                      (lambda (transformer-env)
                        (identifier=? transformer-env 'x env 'x)))))))
              (list (foo)
                    (let ((x 3))
                      (foo)))))

(test-equal '(#f #t)
            (let-syntax ((bar foo))
              (let-syntax
                  ((foo
                    (sc-macro-transformer
                     (lambda (form env)
                       (capture-syntactic-environment
                        (lambda (transformer-env)
                          (identifier=? transformer-env 'foo
                                        env (cadr form))))))))
                (list (foo foo)
                      (foo bar)))))

;; syntactic closure examples, from comp.lang.scheme
;; https://groups.google.com/forum/?fromgroups=#!topic/comp.lang.scheme/JXXH-_DjwtY
(define-syntax push!
  (sc-macro-transformer
   (lambda (exp env)
     (let ((item
            (make-syntactic-closure env '() (cadr exp)))
           (list
            (make-syntactic-closure env '() (caddr exp))))
       `(set! ,list (cons ,item ,list))))))
(define stack '())
(push! 1 stack)
(test-equal '(1) stack)

(define-syntax swap!
  (sc-macro-transformer
   (lambda (form usage-env)
     (let ((a (make-syntactic-closure usage-env '() (cadr form)))
           (b (make-syntactic-closure usage-env '() (caddr form))))
       `(let ((VALUE ,a))
          (set! ,a ,b)
          (set! ,b VALUE))))))
(define a 1)
(define b 2)
(swap! a b)
(test-eqv 2 a)
(test-eqv 1 b)
(test-eqv 1 (let ((set! 5))
              (swap! set! b)
              set!))

(define-syntax aif
  (sc-macro-transformer
   (lambda (form usage-env)
     (let ((condition (make-syntactic-closure usage-env '()
                                              (cadr form)))
           (consequent (make-syntactic-closure usage-env '(it)
                                               (caddr form)))
           (alternative (make-syntactic-closure usage-env '()
                                                (cadddr form))))
       `(let ((it ,condition))
          (if it
              ,consequent
              ,alternative))))))
(test-eqv 'b
          (aif (assv 'a '((1 . 2) (a . b)))
               (cdr it)
               'nothing))
(test-equal '(a . b) ; I think binding 'it' should be closed in aif.
            (let ((it 1))
              (aif (assv 'a '((1 . 2) (a . b)))
                   it
                   'nothing)))

(define-syntax let1
  (sc-macro-transformer
   (lambda (form usage-env)
     (let ((id (cadr form))
           (init (caddr form))
           (exp (cadddr form)))
       `((lambda (,id)
           ,(make-syntactic-closure usage-env (list id)
                                    exp))
         ,(make-syntactic-closure usage-env '()
                                  init))))))
(test-eqv 7 (let1 x 2 (+ x 5)))

(define-syntax loop
  (sc-macro-transformer
   (lambda (exp env)
     (let ((body (cdr exp)))
       `(call-with-current-continuation
         (lambda (exit)
           (let f ()
             ,@(map (lambda (exp)
                       (make-syntactic-closure env '(exit)
                                               exp))
                     body)
             (f))))))))
(define loop-result ())
(let ((x 1))
  (loop (push! x loop-result)
        (if (> x 10)
            (exit x)
            (set! x (+ x 1)))))
(test-equal '(11 10 9 8 7 6 5 4 3 2 1) loop-result)

(test-eqv 11 ; I think binding 'exit' should be closed in aif.
          (let ((x 1)
                (exit 2))
            (loop (push! x loop-result)
                  (if (> x 10)
                      (exit x)
                      (set! x (+ x 1))))))

(define-syntax push
  (sc-macro-transformer
   (lambda (exp usage-env)
     (capture-syntactic-environment
      (lambda (transformer-env)
        (let ((setter
               (make-syntactic-closure transformer-env '() 'set!))
              (item
               (make-syntactic-closure usage-env '() (cadr exp)))
              (list
               (make-syntactic-closure usage-env '() (caddr exp))))
          `(,setter ,list (cons ,item ,list))))))))
(define stack '())
(let ((set! #f))
  (push 1 stack))
(test-equal '(1) stack)


;; ==== syntax-rules ====
(define-syntax push
  (syntax-rules ()
    ((push item list)
     (set! list (cons item list)))))
(set! push-test-lis ())
(test-equal '() push-test-lis)
(push 1 push-test-lis)
(test-equal '(1) push-test-lis)
(push 2 push-test-lis)
(test-equal '(2 1) push-test-lis)

(define-syntax foo
  (syntax-rules ()
    ((_ a ...)
     (list 'a ...))))
(test-equal '(100) (foo 100))
(test-equal '(1 2) (foo 1 2))

;; syntax-rules examples from R5RS
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))
(test-assert cond)
(test-eqv 1 (cond ((eqv? 1 1) 1)))
(test-eqv 3 (cond ((eqv? 1 2) xxx)
                  ((eqv? 2 3) yyy)
                  ((eqv? 3 3) 3)))
(test-eqv 3 (cond ((eqv? 1 2) xxx)
                  ((eqv? 2 3) yyy)
                  (else 3)))
(test-eqv 12 (cond ((eqv? 1 2))
                   ((eqv? 2 3) fuga)
                   ((+ 5 7))))

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))
(test-assert case)
(test-eqv 'odd (case 1
                 ((1 3 5) 'odd)
                 ((2 4 6) 'even)))
(test-eqv 'wakaran (case 'some-symbol
                     ((1 3 5) 'odd)
                     ((2 4 6) 'even)
                     (else 'wakaran)))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))
(test-assert and)
(test-eqv #t (and))
(test-eqv 1 (and 1))
(test-eqv 2 (and 1 2))
(test-eqv #f (and #f 2))
(test-eqv #f (and 1 #f 3))
(test-eqv 3 (and #t #t 3))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))
(test-assert or)
(test-eqv #f (or))
(test-eqv 1 (or 1))
(test-eqv 2 (or #f 2))
(test-eqv 3 (or #f #f 3))
(test-eqv 1 (or 1 #f 3))
(test-eqv 2 (or #f 2 #f 4))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))
(test-eqv 1 (let ((x 1) (y 2) (z 3))
              x))
(test-eqv 2 (let ((x 1) (y 2) (z 3))
              y))
(test-eqv 3 (let ((x 1) (y 2) (z 3))
              z))
(test-eqv 2 (let ((x 1))
              (let ((x 2))
                x)))
(test-eqv 1 (let ((x 1))
              (let ((x 2))
                x)
              x))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))
(test-eqv 1 (let* ((x 1))
              x))
(test-eqv 1 (let* ((x 1) (y x))
              y))
(test-eqv 1 (let* ((x 1) (y x) (z y))
              z))
(test-eqv 2 (let* ((x 1))
              (let ((x 2))
                x)))
(test-eqv 1 (let* ((x 1))
              (let ((x 2))
                x)
              x))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var1 init1) ...) body ...)
     (letrec "generate_temp_names"
       (var1 ...)
       ()
       ((var1 init1) ...)
       body ...))
    ((letrec "generate_temp_names"
       ()
       (temp1 ...)
       ((var1 init1) ...)
       body ...)
     (let ((var1 <undefined>) ...)
       (let ((temp1 init1) ...)
         (set! var1 temp1)
         ...
         body ...)))
    ((letrec "generate_temp_names"
       (x y ...)
       (temp ...)
       ((var1 init1) ...)
       body ...)
     (letrec "generate_temp_names"
       (y ...)
       (newtemp temp ...)
       ((var1 init1) ...)
       body ...))))
(test-eqv 1 (letrec ((x 1))
              x))
(test-eqv 2 (letrec ((x 1) (y 2))
              y))
(test-eqv 3 (letrec ((x 1) (y 2) (z 3))
              z))
(test-eqv 2 (letrec ((x 1))
              (let ((x 2))
                x)))
(test-eqv 1 (letrec ((x 1))
              (let ((x 2))
                x)
              x))
(test-eq #t
         (letrec ((even? (lambda (n)
                           (if (zero? n) #t (odd? (- n 1)))))
                  (odd?  (lambda (n)
                           (if (zero? n) #f (even? (- n 1))))))
           (even? 88)))

(define-syntax begin
  (syntax-rules ()
    ((begin exp ...)
     ((lambda () exp ...)))))
(test-eqv 1 (begin 1))
(test-eqv 2 (begin 1 2))
(test-eqv 3 (begin 1 2 3))

(define-syntax begin
  (syntax-rules ()
    ((begin exp)
     exp)
    ((begin exp1 exp2 ...)
     (let ((x exp1))
       (begin exp2 ...)))))
(test-eqv 1 (begin 1))
(test-eqv 2 (begin 1 2))
(test-eqv 3 (begin 1 2 3))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (letrec
         ((loop
           (lambda (var ...)
             (if test
                 (begin
                   (if #f #f)
                   expr ...)
                 (begin
                   command
                   ...
                   (loop (do "step" var step ...)
                         ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))
(test-equal '#(0 1 2 3 4)
            (do ((vec (make-vector 5))
                 (i 0 (+ i 1)))
                ((= i 5) vec)
              (vector-set! vec i i)))


(zs-test-report)
