(load "zs_test_util.scm")

(test-eq #t (eqv? 'a 'a))
(test-eq #f (eqv? 'a 'b))
(test-eq #t (eqv? 2 2))
(test-eq #t (eqv? '() '()))
(test-eq #t (eqv? 100000000 100000000))
(test-eq #f (eqv? (cons 1 2) (cons 1 2)))
(test-eq #f (eqv? (lambda () 1) (lambda () 2)))
(test-eq #f (eqv? #f 'nil))
(test-eq #t (let ((p (lambda (x) x)))
               (eqv? p p)))

(test-eq #f (eqv? "" "")) ; unspecified
(test-eq #f (eqv? '#() '#())) ; unspecified
(test-eq #f (eqv? (lambda (x) x) (lambda (x) x))) ; unspecified
(test-eq #f (eqv? (lambda (x) x) (lambda (y) y))) ; unspecified

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(test-eq #t (let ((g (gen-counter)))
               (eqv? g g)))
(test-eq #f (eqv? (gen-counter) (gen-counter)))

(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))
(test-eq #t (let ((g (gen-loser)))
               (eqv? g g)))
(test-eq #f (eqv? (gen-loser) (gen-loser))) ; unspecified

(test-eq #f (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
                     (g (lambda () (if (eqv? f g) 'both 'g))))
              (eqv? f g)))
(test-eq #f (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                     (g (lambda () (if (eqv? f g) 'g 'both))))
              (eqv? f g)))
(test-eq #f (eqv? '(a) '(a))) ; unspecified
(test-eq #f (eqv? "a" "a")) ; unspecified
(test-eq #f (eqv? '(b) (cdr '(a b)))) ; unspecified
(test-eq #t (let ((x '(a)))
               (eqv? x x)))


(test-eq #t (eq? 'a 'a))
(test-eq #f (eq? '(a) '(a))) ; unspecified
(test-eq #f (eq? (list 'a) (list 'a)))
(test-eq #f (eq? "a" "a")) ; unspecified
(test-eq #f (eq? "" "")) ; unspecified
(test-eq #t (eq? '() '()))
(test-eq #t (eq? 2 2)) ; unspecified
(test-eq #t (eq? #\A #\A)) ; unspecified
(test-eq #t (eq? car car))
(test-eq #t (let ((n (+ 2 3)))
               (eq? n n))) ; unspecified
(test-eq #t (let ((x '(a)))
               (eq? x x)))
(test-eq #t (let ((x '#()))
               (eq? x x)))
(test-eq #t (let ((p (lambda (x) x)))
               (eq? p p)))


(test-eq #t (equal? 'a 'a))
(test-eq #t (equal? '(a) '(a)))
(test-eq #t (equal? '(a (b) c) '(a (b) c)))
(test-eq #t (equal? "abc" "abc"))
(test-eq #t (equal? 2 2))
(test-eq #t (equal? (make-vector 5 'a) (make-vector 5 'a)))
(test-eq #f (equal? (lambda (x) x) (lambda (y) y))) ; unspecified


(zs-test-report)
