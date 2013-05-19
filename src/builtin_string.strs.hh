// This file is intended to be included into an array of 'const char*'

"(define (string-fill! str fill)"
"  (let ((size (string-length str)))"
"    (do ((i 0 (+ i 1)))"
"        ((= i size) str)"
"      (string-set! str i fill))))",
