// This file is intended to be included into an array of 'const char*'

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

"(define (abs n)"
"  (if (negative? n) (- n) n))",
