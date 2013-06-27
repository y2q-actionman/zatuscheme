(load "zs_test_util.scm")

;;; SRFI-0
(test-eq 'ok
         (cond-expand (srfi-0
                       'ok)
                      (else 'ng)))

(test-eq 'ng
         (cond-expand (else 'ng)))

(test-eq 'ng
         (cond-expand (hoge 'ok)
                      (else 'ng)))

(test-eq 'ok
         (cond-expand ((and srfi-0 srfi-0)
                       'ok)
                      (else 'ng)))

(test-eq 'ng
         (cond-expand ((and srfi-0 hoge)
                       'ok)
                      (else 'ng)))

(test-eq 'ok
         (cond-expand ((or srfi-0 hoge)
                       'ok)
                      (else 'ng)))

(test-eq 'ng
         (cond-expand ((or (not srfi-0) hoge)
                       'ok)
                      (else 'ng)))

(zs-test-report)
