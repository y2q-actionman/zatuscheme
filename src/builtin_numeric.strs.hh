// This file is intended to be included into an array of 'const char*'

"(define (%fold proc init lis)"
"  (if (null? lis) init"
"    (%fold proc (apply proc init (car lis)) (cdr lis))))",

"(define (inexact? n)"
"  (if (number? n) (not (exact? n)) #f))",

#define NUMBER_CMP_FUNCS(op)                  \
  "(define ("op" n1 n2 . n3)"                 \
  "  (and (%"op" n1 n2)"                      \
  "       (or (null? n3)"                     \
  "           (apply "op" (list n2) n3))))"
NUMBER_CMP_FUNCS("="),
NUMBER_CMP_FUNCS("<"),
NUMBER_CMP_FUNCS(">"),
NUMBER_CMP_FUNCS("<="),
NUMBER_CMP_FUNCS(">="),
#undef NUMBER_CMP_FUNCS

"(define (zero? n)"
"  (if (number? n) (= n 0) #f))",

"(define (positive? n)"
"  (if (real? n) (> n 0) #f))",

"(define (negative? n)"
"  (if (real? n) (< n 0) #f))",

"(define (odd? n)"
"  (if (integer? n) (= 1 (modulo n 2)) #f))",

"(define (even? n)"
"  (if (integer? n) (= 0 (modulo n 2)) #f))",

"(define (max n . m)"
"  (%fold %max n m))",

"(define (min n . m)"
"  (%fold %min n m))",

"(define (+ . n)"
"  (%fold %+ 0 n))",

"(define (* . n)"
"  (%fold %* 1 n))",

"(define (abs n)"
"  (if (negative? n) (- n) n))",
