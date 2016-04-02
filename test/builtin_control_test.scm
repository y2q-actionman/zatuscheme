(load "zs_test_util.scm")

;; control funcs
(test-eqv 7 (apply + (list 3 4)))
(test-eqv 10 (apply + 3 4 (list 1 2)))

(define compose
  (lambda (f g)
    (lambda args (f (apply g args)))))
(test-approximate 30 
                  ((compose sqrt *) 12 75)
                  0.0001)

(test-equal '(b e h)
            (map cadr '((a b) (d e) (g h))))
(test-equal '(1 4 27 256 3125)
            (map (lambda (n) (expt n n)) '(1 2 3 4 5)))
(test-equal '(5 7 9)
            (map + '(1 2 3) '(4 5 6)))
(test-equal '(1 2)
            (let ((count 0))
              (map (lambda (ignored)
                     (set! count (+ count 1))
                     count)
                   '(a b))))

(test-equal '#(0 1 4 9 16)
            (let ((v (make-vector 5)))
              (for-each (lambda (i) (vector-set! v i (* i i)))
                        '(0 1 2 3 4))
              v))

(test-eqv 5 (call-with-values
                (lambda () (values 4 5))
              (lambda (a b) b)))
(test-eqv -1 (call-with-values * -))

(test-eqv 1 (call-with-current-continuation (lambda (c) (c 1))))

(define list-length
  (lambda (obj) (call-with-current-continuation
                 (lambda (return)
                   (letrec ((r (lambda (obj)
                                 (cond ((null? obj) 0)
                                       ((pair? obj) (+ (r (cdr obj)) 1))
                                       (else (return #f))))))
                     (r obj))))))
(test-eqv 4 (list-length '(1 2 3 4)))
(test-eqv #f (list-length '(a b . c)))

(test-eqv 111
          (let ((n 0))
            (dynamic-wind
                (lambda () (set! n (+ n 1)))
                (lambda () (set! n (+ n 10)))
                (lambda () (set! n (+ n 100))))
            n))

(test-equal '(connect talk1 disconnect connect talk2 disconnect)
            (let ((path '())
                  (c #f))
              (let ((add (lambda (s)
                           (set! path (cons s path)))))
                (dynamic-wind
                    (lambda () (add 'connect))
                    (lambda ()
                      (add (call-with-current-continuation
                            (lambda (c0)
                              (set! c c0)
                              'talk1))))
                    (lambda () (add 'disconnect)))
                (if (< (length path) 4)
                    (c 'talk2)
                    (reverse path)))))


(zs-test-report)
