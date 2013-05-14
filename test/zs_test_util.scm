(define zs-test-fail-count 0)

(define (zs-test-report)
  (if (= zs-test-fail-count 0)
      #t
      (begin (display "failed tests: ")
             (display zs-test-fail-count)
             (newline)
             #f)))

;; references SRFI-64.

(define (zs-test-fail form)
  (set! zs-test-fail-count (+ zs-test-fail-count 1))
  (display "[failed] ")
  (display form)
  (newline))

(define-syntax test-assert
  (traditional-transformer
   (lambda (expr)
     `(if ,expr
          #t
          (zs-test-fail ',expr)))))

(define-syntax test-eqv
  (traditional-transformer
   (lambda (expected expr)
     `(test-assert (eqv? ,expected ,expr)))))

(define-syntax test-equal
  (traditional-transformer
   (lambda (expected expr)
     `(test-assert (equal? ,expected ,expr)))))

(define-syntax test-eq
  (traditional-transformer
   (lambda (expected expr)
     `(test-assert (eq? ,expected ,expr)))))

(define-syntax test-approximate
  (traditional-transformer
   (lambda (expected expr error)
     `(let ((expected-val ,expected)
            (expr-val ,expr)
            (error-val ,error))
        (if (<= (- expected-val error-val)
                expr-val 
                (+ expected-val error-val))
            #t
            (zs-test-fail '(<= (- ,expected ,error)
                               ,expr 
                               (+ ,expected ,error))))))))

;; TODO: use this!
(define (with-ignoring-errors thunk)
  (with-exception-handler (lambda (_) #f) thunk))

(define-syntax test-error
  (traditional-transformer
   (lambda (expr)
     `(if (not ,expr)
          #t
          (zs-test-fail '(not ,expr))))))
