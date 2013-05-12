(load "zs_test_util.scm")

;; stdin. assumes no input is supplied.
(let loop ()
  (if (char-ready?)
      (begin (read-char) (loop))
      #f))
(test-eq #f (char-ready?))

;; regular file. 
(define tmpf-in #f)
(define tmpf-out #f)
(let ((ports (tmp-file)))
  (set! tmpf-in (car ports))
  (set! tmpf-out (cadr ports)))

(write-char #\a tmpf-out)
(close-output-port tmpf-out)

(test-eq #t (char-ready? tmpf-in))
(test-eq #\a (read-char tmpf-in))

;; edge case. fd points the end-of-file, just.
;; (#t (char-ready? tmpf-in))

(test-eq #t (eof-object? (read-char tmpf-in)))
(test-eq #t (char-ready? tmpf-in))
(test-eq #t (eof-object? (read-char tmpf-in)))
(test-eq #t (char-ready? tmpf-in))
(close-input-port tmpf-in)


(zs-test-report)
