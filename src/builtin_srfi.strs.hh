// This file is intended to be included into an array of 'const char*'

"(define (ignore-errors thunk)"
"  (with-exception-handler (lambda (_) #f) thunk))",
