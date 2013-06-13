LOAD
(define (newline . args)
  (apply write-char (quote (#\\newline)) args))

LOAD
(define (current-input-port) *current-input-port-value*)

LOAD
(define (current-output-port) *current-output-port-value*)

LOAD
(define (call-with-input-file string proc)
  (let* ((port (open-input-file string))
         (ret (proc port)))
    (close-input-port port)
    ret))

LOAD
(define (call-with-output-file string proc)
  (let* ((port (open-output-file string))
         (ret (proc port)))
    (close-output-port port)
    ret))

LOAD
(define (with-input-from-file string thunk)
  (let ((old-port (current-input-port)))
    (dynamic-wind
        (lambda () (set! *current-input-port-value* (open-input-file string)))
        thunk
        (lambda () (close-input-port (current-input-port))
                (set! *current-input-port-value* old-port)))))

LOAD
(define (with-output-to-file string thunk)
  (let ((old-port (current-output-port)))
    (dynamic-wind 
        (lambda () (set! *current-output-port-value* (open-output-file string)))
        thunk
        (lambda () (close-output-port (current-output-port))
                (set! *current-output-port-value* old-port)))))
