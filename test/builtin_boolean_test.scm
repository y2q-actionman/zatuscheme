(load "zs_test_util.scm")

(test-eq #f (not #t))
(test-eq #f (not 3))
(test-eq #f (not (list 3)))
(test-eq #t (not #f))
(test-eq #f (not '()))
(test-eq #f (not (list)))
(test-eq #f (not 'nil))

(test-assert (boolean? #f))
(test-error (boolean? 0))
(test-error (boolean? '()))

(zs-test-report)
