// This file is intended to be included into an array of 'const char*'

"(define (vector-fill! vector fill)"
"  (do ((size (vector-length vector) size)"
"       (i 0 (+ i 1)))"
"      ((= i size) vector)"
"    (vector-set! vector i fill)))",
