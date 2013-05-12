(load "zs_test_util.scm")

(test-eqv 3 (force (delay (+ 1 2))))
(test-equal '(3 3)
            (let ((p (delay (+ 1 2))))
              (list (force p) (force p))))

(define a-stream
  (letrec ((next (lambda (n)
                   (cons n (delay (next (+ n 1)))))))
    (next 0)))

(define head car)
(define tail (lambda (stream) (force (cdr stream))))
(test-eqv 2 (head (tail (tail a-stream))))

(define count 0)
(define p (delay (begin (set! count (+ count 1))
                        (if (> count x) count (force p)))))
(define x 5)
(test-eqv 6 (force p))
(test-eqv 6 (begin (set! x 10) (force p)))


(zs-test-report)
