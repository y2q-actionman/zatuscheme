// This file is intended to be included into an array of 'const char*'

"(define (with-exception-handler handler thunk)"
"  (let ((old-handler "CURRENT_EXCEPTION_HANDLER_SYMNAME"))"
"    (dynamic-wind (lambda () (set! "CURRENT_EXCEPTION_HANDLER_SYMNAME" handler))"
"                  thunk"
"                  (lambda () (set! "CURRENT_EXCEPTION_HANDLER_SYMNAME" old-hanlder)))))",
