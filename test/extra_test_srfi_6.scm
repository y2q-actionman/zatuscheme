(load "zs_test_util.scm")

;;; SRFI-6
(define p
  (open-input-string "(a . (b . (c . ()))) 34"))

(test-eq #t
	 (input-port? p))

(test-equal '(a b c)
	    (read p))

(test-equal 34
	    (read p))

(test-eq #t
	 (eof-object? (peek-char p)))

(test-equal "a(b c)"
            (let ((q (open-output-string))
                  (x '(a b c)))
              (write (car x) q)
              (write (cdr x) q)
              (get-output-string q)))

(zs-test-report)
