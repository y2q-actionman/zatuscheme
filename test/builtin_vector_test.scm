(load "zs_test_util.scm")

(test-eq #t (vector? '#()))
(test-eq #t (vector? '#(1)))
(test-eq #f (vector? 'foo))

(test-equal '#(a a a) (make-vector 3 'a))
(test-equal '#(a a a a a) (make-vector 5 'a))

(test-equal #(1) (vector 1))
(test-equal #(1 2) (vector 1 2))
(test-equal #(1 2 3) (vector 1 2 3))
(test-equal '#(a) (vector 'a))
(test-equal '#(a b c) (vector 'a 'b 'c))

(test-equal 0 (vector-length '#()))
(test-equal 1 (vector-length '#(1)))
(test-equal 3 (vector-length '#(1 2 3)))

(test-equal 'a (vector-ref '#(a) 0))
(test-equal 'c (vector-ref '#(a b c d e) 2))
(test-error (vector-ref '#(a) -1))
(test-error (vector-ref '#(a) 100))
(test-error (vector-ref '#() 0))

(test-equal 8 (vector-ref '#(1 1 2 3 5 8 13 21) 5))
(test-equal 13 (vector-ref '#(1 1 2 3 5 8 13 21)
                           (let ((i (round (* 2 (acos -1)))))
                             (if (inexact? i) (inexact->exact i) i))))

(test-equal #(0 ("Sue" "Sue") "Anna")
            (let ((vec (vector 0 '(2 2 2 2) "Anna")))
              (vector-set! vec 1 '("Sue" "Sue")) vec))
;; (test-error (vector-set! '#(0 1 2) 1 "doe"))

(define tmpvec (vector 1))
(test-error (vector-set! tmpvec -1 'hoge))
(test-error (vector-set! tmpvec 100 'hoge))

(test-equal '(dah dah didah) (vector->list '#(dah dah didah)))
(test-equal '#(dididit dah) (list->vector '(dididit dah)))

(define tmpv (vector 1 2 3))
(vector-fill! tmpv '?)
(test-equal '#(? ? ?) tmpv)
(vector-fill! tmpv '!)
(test-equal '#(! ! !) tmpv)


(zs-test-report)
