(load "zs_test_util.scm")

;;; from SRFI-34 examples

;; PRINTS: condition: an-error
(test-eq 'exception
         (call-with-current-continuation
          (lambda (k)
            (with-exception-handler
             (lambda (x)
               (display "condition: ")
               (write x)
               (newline)
               (k 'exception))
             (lambda ()
               (+ 1 (raise 'an-error)))))))

;; ;; PRINTS: something went wrong
;; (test-error 
;;  (call-with-current-continuation
;;   (lambda (k)
;;     (with-exception-handler
;;      (lambda (x)
;;        (display "something went wrong")
;;        (newline)
;;        'dont-care)
;;      (lambda ()
;;        (+ 1 (raise 'an-error)))))))

;; PRINTS: condition: an-error
(test-eq 'exception
         (guard (condition
                 (else
                  (display "condition: ")
                  (write condition)
                  (newline)
                  'exception))
                (+ 1 (raise 'an-error))))

;; PRINTS: something went wrong
(test-eq 'dont-care
         (guard (condition
                 (else
                  (display "something went wrong")
                  (newline)
                  'dont-care))
                (+ 1 (raise 'an-error))))

(test-eq 'positive
         (call-with-current-continuation
          (lambda (k)
            (with-exception-handler 
             (lambda (x)
               (display "reraised ") (write x) (newline)
               (k 'zero))
             (lambda ()
               (guard (condition
                       ((positive? condition) 'positive)
                       ((negative? condition) 'negative))
                      (raise 1)))))))

(test-eq 'negative
         (call-with-current-continuation
          (lambda (k)
            (with-exception-handler
             (lambda (x)
               (display "reraised ") (write x) (newline)
               (k 'zero))
             (lambda ()
               (guard (condition
                       ((positive? condition) 'positive)
                       ((negative? condition) 'negative))
                      (raise -1)))))))

;; PRINTS: reraised 0
(test-eq 'zero
         (call-with-current-continuation
          (lambda (k)
            (with-exception-handler
             (lambda (x)
               (display "reraised ") (write x) (newline)
               (k 'zero))
             (lambda ()
               (guard (condition
                       ((positive? condition) 'positive)
                       ((negative? condition) 'negative))
                      (raise 0)))))))

(test-eqv 42
          (guard (condition
                  ((assq 'a condition) => cdr)
                  ((assq 'b condition)))
                 (raise (list (cons 'a 42)))))

(test-equal '(b . 23)
            (guard (condition
                    ((assq 'a condition) => cdr)
                    ((assq 'b condition)))
                   (raise (list (cons 'b 23)))))


(zs-test-report)
