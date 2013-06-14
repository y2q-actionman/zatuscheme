LOAD
(define (newline . args)
  (apply write-char (string-ref "\n" 0) args))

LOAD
(define (current-input-port) CURRENT_INPUT_PORT_SYMNAME)

LOAD
(define (current-output-port) CURRENT_OUTPUT_PORT_SYMNAME)

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
        (lambda () (set! CURRENT_INPUT_PORT_SYMNAME (open-input-file string)))
        thunk
        (lambda () (close-input-port (current-input-port))
                (set! CURRENT_INPUT_PORT_SYMNAME old-port)))))

LOAD
(define (with-output-to-file string thunk)
  (let ((old-port (current-output-port)))
    (dynamic-wind 
        (lambda () (set! CURRENT_OUTPUT_PORT_SYMNAME (open-output-file string)))
        thunk
        (lambda () (close-output-port (current-output-port))
                (set! CURRENT_OUTPUT_PORT_SYMNAME old-port)))))
