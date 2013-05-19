(load "zs_test_util.scm")

;; syntaxes
(test-eqv #t (and))
(test-eqv 1 (and 1))
(test-eqv 2 (and 1 2))
(test-eqv #f (and #f 2))
(test-eqv #f (and 1 #f 3))
(test-eqv 3 (and #t #t 3))

(test-eqv #f (or))
(test-eqv 1 (or 1))
(test-eqv 2 (or #f 2))
(test-eqv 3 (or #f #f 3))
(test-eqv 1 (or 1 #f 3))
(test-eqv 2 (or #f 2 #f 4))

(test-eqv 100 (let () 100))
;; (test-error (let (x) x))                ; not '()
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

;; TODO: add more named-let patterns
(test-assert (let loop ((x #f)) (if x x (loop #t))))
;; (test-error (let 100 ((x 0))
;;               x))
;; (test-assert (let loop ((x #f)) (if x x (loop #f)))) // infinite loop!

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
(test-eqv #t (letrec ((even? (lambda (n)
                               (if (zero? n) #t (odd? (- n 1)))))
                      (odd?  (lambda (n)
                               (if (zero? n) #f (even? (- n 1))))))
               (even? 88)))

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

(test-eqv 'odd (case 1
                 ((1 3 5) 'odd)
                 ((2 4 6) 'even)))
(test-eqv 'wakaran (case 'some-symbol
                     ((1 3 5) 'odd)
                     ((2 4 6) 'even)
                     (else 'wakaran)))

(test-eqv 1 (eval 1 (null-environment 5)))
(test-eqv 4 (eval (+ 1 3) (scheme-report-environment 5)))
(test-eqv 4 (eval '(+ 1 3) (scheme-report-environment 5)))
(test-equal "different" (eval '(if (eqv? 1 2) "same" "different")
                              (interaction-environment)))

(test-equal #(0 1 2 3 4)
            (do ((vec (make-vector 5))
                 (i 0 (+ i 1)))
                ((= i 5) vec)
              (vector-set! vec i i)))


(zs-test-report)