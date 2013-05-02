// This file is intended to be included into an array of 'const char*'

"(define (inexact? n)"
"  (if (number? n) (not (exact? n)) #f))",

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
