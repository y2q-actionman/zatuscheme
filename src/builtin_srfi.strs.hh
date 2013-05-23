// This file is intended to be included into an array of 'const char*'

"(define (ignore-errors thunk)"
"  (call-with-current-continuation"
"    (lambda (cont)"
"      (with-exception-handler (lambda (e) (cont #f e))"
"        thunk))))",                       
