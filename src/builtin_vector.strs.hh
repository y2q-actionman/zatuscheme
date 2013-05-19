// This file is intended to be included into an array of 'const char*'

"(define (vector-fill! vec fill)"
"  (let ((size (vector-length vec)))"
"    (do ((i 0 (+ i 1)))"
"        ((= i size) vec)"
"      (vector-set! vec i fill))))",
