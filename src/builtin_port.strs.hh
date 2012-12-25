// This file is intended to be included into an array of 'const char*'

"(define newline (lambda args (apply write-char '(#\\newline) args)))",

"(define (current-input-port) "CURRENT_INPUT_PORT_SYMNAME")",
"(define (current-output-port) "CURRENT_OUTPUT_PORT_SYMNAME")",

"(define (call-with-input-file string proc)"
"  (let* ((port (open-input-file string))"
"         (ret (proc port)))"
"    (close-input-port port)"
"    ret))",

"(define (call-with-output-file string proc)"
"  (let* ((port (open-output-file string))"
"         (ret (proc port)))"
"    (close-output-port port)"
"    ret))",

"(define (with-input-from-file string thunk)"
"  (let ((old-port (current-input-port)))"
"    (dynamic-wind (lambda () (set! "CURRENT_INPUT_PORT_SYMNAME" (open-input-file string)))"
"                  thunk"
"                  (lambda () (close-input-port "CURRENT_INPUT_PORT_SYMNAME")"
"                             (set! "CURRENT_INPUT_PORT_SYMNAME" old-port)))))",

"(define (with-output-to-file string thunk)"
"  (let ((old-port (current-output-port)))"
"    (dynamic-wind (lambda () (set! "CURRENT_OUTPUT_PORT_SYMNAME" (open-output-file string)))"
"                  thunk"
"                  (lambda () (close-output-port "CURRENT_OUTPUT_PORT_SYMNAME")"
"                             (set! "CURRENT_OUTPUT_PORT_SYMNAME" old-port)))))",
