(load "zs_test_util.scm")

(test-eq #f (not #t))
(test-eq #f (not 3))
(test-eq #f (not (list 3)))
(test-eq #t (not #f))
(test-eq #f (not '()))
(test-eq #f (not (list)))
(test-eq #f (not 'nil))

(test-eq #t (boolean? #f))
(test-eq #f (boolean? 0))
(test-eq #f (boolean? '()))

(zs-test-report)
