LOAD
(define (newline . args)
  (apply write-char NEWLINE_CHAR args))

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

LOAD
(define (%read-funcs fun)
  (lambda args (apply fun (if (null? args) (current-input-port) args))))

LOAD
(define read (%read-funcs %read))
LOAD
(define read-char (%read-funcs %read-char))
LOAD
(define peek-char (%read-funcs %peek-char))
LOAD
(define char-ready? (%read-funcs %char-ready?))

LOAD
(define (%write-funcs fun)
  (lambda (p . args) (apply fun `(,p) (if (null? args) (current-output-port) args))))

LOAD
(define write (%write-funcs %write))
LOAD
(define display (%write-funcs %display))
LOAD
(define write-char (%write-funcs %write-char))
