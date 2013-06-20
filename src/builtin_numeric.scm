LOAD
(define (%fold proc init lis)
  (if (null? lis) init
      (%fold proc (apply proc init (car lis)) (cdr lis))))

LOAD
(define (inexact? n)
  (if (number? n) (not (exact? n)) #f))

LOAD
(define (%number-cmp op)
  (letrec ((fun
            (lambda (n1 n2 . n3)
              (and (op n1 n2)
                   (or (null? n3)
                       (apply fun n2 n3))))))
    fun))

LOAD
(define = (%number-cmp %=))
LOAD
(define < (%number-cmp %<))
LOAD
(define > (%number-cmp %>))
LOAD
(define <= (%number-cmp %<=))
LOAD
(define >= (%number-cmp %>=))

LOAD
(define (zero? n)
  (if (number? n) (= n 0) #f))

LOAD
(define (positive? n)
  (if (real? n) (> n 0) #f))

LOAD
(define (negative? n)
  (if (real? n) (< n 0) #f))

LOAD
(define (odd? n)
  (if (integer? n) (= 1 (modulo n 2)) #f))

LOAD
(define (even? n)
  (if (integer? n) (= 0 (modulo n 2)) #f))

LOAD
(define (max n . m)
  (%fold %max n m))

LOAD
(define (min n . m)
  (%fold %min n m))

LOAD
(define (+ . n)
  (%fold %+ 0 n))

LOAD
(define (* . n)
  (%fold %* 1 n))

LOAD
(define (- n . m)
  (if (null? m) (%- 0 n)
      (%fold %- n m)))

LOAD
(define (/ n . m)
  (if (null? m) (%/ 1 n)
      (%fold %/ n m)))

LOAD
(define (abs n)
  (if (negative? n) (- n) n))

LOAD
(define (atan n . m)
  (if (null? m) (%atan1 n) (apply %atan2 `(,n) m)))

LOAD
(define (gcd . n)
  (%fold %gcd 0 n))

LOAD
(define (lcm . n)
  (%fold %lcm 1 n))
