(load "zs_test_util.scm")

(test-assert (eqv? 'a 'a))
(test-error (eqv? 'a 'b))
(test-assert (eqv? 2 2))
(test-assert (eqv? '() '()))
(test-assert (eqv? 100000000 100000000))
(test-error (eqv? (cons 1 2) (cons 1 2)))
(test-error (eqv? (lambda () 1) (lambda () 2)))
(test-error (eqv? #f 'nil))
(test-assert (let ((p (lambda (x) x)))
               (eqv? p p)))

(test-error (eqv? "" "")) ; unspecified
(test-error (eqv? '#() '#())) ; unspecified
(test-error (eqv? (lambda (x) x) (lambda (x) x))) ; unspecified
(test-error (eqv? (lambda (x) x) (lambda (y) y))) ; unspecified

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(test-assert (let ((g (gen-counter)))
               (eqv? g g)))
(test-error (eqv? (gen-counter) (gen-counter)))

(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))
(test-assert (let ((g (gen-loser)))
               (eqv? g g)))
(test-error (eqv? (gen-loser) (gen-loser))) ; unspecified

(test-error (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
                     (g (lambda () (if (eqv? f g) 'both 'g))))
              (eqv? f g)))
(test-error (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                     (g (lambda () (if (eqv? f g) 'g 'both))))
              (eqv? f g)))
(test-error (eqv? '(a) '(a))) ; unspecified
(test-error (eqv? "a" "a")) ; unspecified
(test-error (eqv? '(b) (cdr '(a b)))) ; unspecified
(test-assert (let ((x '(a)))
               (eqv? x x)))


(test-assert (eq? 'a 'a))
(test-error (eq? '(a) '(a))) ; unspecified
(test-error (eq? (list 'a) (list 'a)))
(test-error (eq? "a" "a")) ; unspecified
(test-error (eq? "" "")) ; unspecified
(test-assert (eq? '() '()))
(test-assert (eq? 2 2)) ; unspecified
(test-assert (eq? #\A #\A)) ; unspecified
(test-assert (eq? car car))
(test-assert (let ((n (+ 2 3)))
               (eq? n n))) ; unspecified
(test-assert (let ((x '(a)))
               (eq? x x)))
(test-assert (let ((x '#()))
               (eq? x x)))
(test-assert (let ((p (lambda (x) x)))
               (eq? p p)))


(test-assert (equal? 'a 'a))
(test-assert (equal? '(a) '(a)))
(test-assert (equal? '(a (b) c) '(a (b) c)))
(test-assert (equal? "abc" "abc"))
(test-assert (equal? 2 2))
(test-assert (equal? (make-vector 5 'a) (make-vector 5 'a)))
(test-error (equal? (lambda (x) x) (lambda (y) y))) ; unspecified


(zs-test-report)
