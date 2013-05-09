(load "zs_test_util.scm")

;; (test-error ,@(1))
;; (test-error `(,@1))
(test-equal () `(,@()))

(test-equal 1 `1)
(test-equal 1 `,1)
(test-equal #(1) `#(1))
(test-equal () `())
(test-equal '(1) `(1))
(test-equal '(1) `(,1))

(define (retlist) (list 1 2 3))
(test-equal '(1 2 3) (retlist))
(test-equal '(0 (1 2 3)) `(0,(retlist)))
(test-equal '(1 2 3) `(,@(retlist)))

(test-equal '(()) `(,()))
(test-equal '(() 1 2 () 3) `(,() 1 2 ,() 3))
(test-equal '(1 2 () 3) `(,@() 1 2 ,() 3))

(test-equal '(()) `(,()))
(test-equal '((1)) `(,`(1)))
(test-equal '((1 2)) `(,`(1 ,(+ 1 1))))
(test-equal '((1 2 3 4 5)) `(,`(1 ,(+ 1 1) ,@(list 3 4 5))))


(zs-test-report)
