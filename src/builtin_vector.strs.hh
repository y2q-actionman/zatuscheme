// This file is intended to be included into an array of 'const char*'

"(define (vector-fill! vec fill)"
"  (let ((size (vector-length vec)))"
"    (do ((i 0 (+ i 1)))"
"        ((= i size) vec)"
"      (vector-set! vec i fill))))",

"(define (vector->list vec)"
"  (let ((size (vector-length vec)))"
"    (let loop ((i 0) (lis ()))"
"       (if (= i size) (reverse lis)"
"         (loop (+ i 1) (cons (vector-ref vec i) lis))))))",

"(define (list->vector lis)"
"  (apply vector lis))",
