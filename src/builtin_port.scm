LOAD
(define input-port? %input-port?)
LOAD
(define output-port? %output-port?)
LOAD
(define open-input-file %open-input-file)
LOAD
(define open-output-file %open-output-file)
LOAD
(define close-input-port %close-input-port)
LOAD
(define close-output-port %close-output-port)

LOAD
(define (call-with-input-file string proc)
  (let ((port %undefined))
    (dynamic-wind
	(lambda () (set! port (open-input-file string)))
	(lambda () (proc port))
	(lambda () (close-input-port port)))))

LOAD
(define (call-with-output-file string proc)
  (let ((port %undefined))
    (dynamic-wind
	(lambda () (set! port (open-output-file string)))
	(lambda () (proc port))
	(lambda () (close-output-port port)))))

LOAD
(define (current-input-port) CURRENT_INPUT_PORT_SYMNAME)
LOAD
(define (current-output-port) CURRENT_OUTPUT_PORT_SYMNAME)

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
  (lambda args
    (if (null? args) (fun (current-input-port))
	(apply fun args))))
LOAD
(define read (%read-funcs %read))
LOAD
(define read-char (%read-funcs %read-char))
LOAD
(define peek-char (%read-funcs %peek-char))
LOAD
(define char-ready? (%read-funcs %char-ready?))

LOAD
(define eof-object? %eof-object?)

LOAD
(define (%write-funcs fun)
  (lambda (p . args)
    (if (null? args) (fun p (current-output-port))
	(apply fun p args))))
LOAD
(define write (%write-funcs %write))
LOAD
(define display (%write-funcs %display))
LOAD
(define write-char (%write-funcs %write-char))

LOAD
(define (newline . args)
  (apply write-char NEWLINE_CHAR args))
