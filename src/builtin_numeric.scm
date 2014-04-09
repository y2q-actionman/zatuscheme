LOAD
(define number? %number?)

LOAD
(define complex? %complex?)

LOAD
(define (real? obj)
  (or (%real? obj)
      (and (%complex? obj)
	   (%= 0 (%imag-part obj)))))

LOAD
(define rational? %rational?)

LOAD
(define (integer? obj)
  (or (%integer? obj)
      (and (real? obj)
	   (%= (%real-part obj) (%round (%real-part obj))))))

LOAD
(define exact? %exact?)

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
(define (%> n1 n2) (%< n2 n1))
LOAD
(define > (%number-cmp %>))

LOAD
(define (%<= n1 n2) (not (%< n2 n1)))
LOAD
(define <= (%number-cmp %<=))

LOAD
(define (%>= n1 n2) (not (%< n1 n2)))
LOAD
(define >= (%number-cmp %>=))

LOAD
(define (zero? n)
  (if (number? n) (= n 0) #f))

LOAD
(define (positive? n)
  (if (%real? n) (> n 0) #f))

LOAD
(define (negative? n)
  (if (%real? n) (< n 0) #f))

LOAD
(define (odd? n)
  (if (%integer? n) (= 1 (modulo n 2)) #f))

LOAD
(define (even? n)
  (if (%integer? n) (= 0 (modulo n 2)) #f))

LOAD
(define (%fold proc init lis)
  (if (null? lis) init
      (%fold proc (apply proc init (car lis)) (cdr lis))))

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
(define quotient %quotient)
LOAD
(define remainder %remainder)
LOAD
(define modulo %modulo)

LOAD
(define (gcd . n)
  (%fold %gcd 0 n))

LOAD
(define (%lcm n m)
  (abs (/ (%* n m) (%gcd n m))))
LOAD
(define (lcm . n)
  (%fold %lcm 1 n))

LOAD
(define numerator %numerator)
LOAD
(define denominator %denominator)

LOAD
(define floor %floor)
LOAD
(define ceiling %ceiling)
LOAD
(define truncate %truncate)
LOAD
(define round %round)

LOAD
(define rationalize %rationalize)

LOAD
(define exp %exp)
LOAD
(define log %log)
LOAD
(define sin %sin)
LOAD
(define cos %cos)
LOAD
(define tan %tan)
LOAD
(define asin %asin)
LOAD
(define acos %acos)
LOAD
(define (atan n . m)
  (if (null? m) (%atan1 n) (apply %atan2 `(,n) m)))

LOAD
(define sqrt %sqrt)

LOAD
(define expt %expt)

LOAD
(define make-rectangular %make-rectangular)
LOAD
(define make-polar %make-polar)
LOAD
(define real-part %real-part)
LOAD
(define imag-part %imag-part)
LOAD
(define magnitude %magnitude)
LOAD
(define angle %angle)

LOAD
(define exact->inexact %exact->inexact)
LOAD
(define inexact->exact %inexact->exact)

LOAD
(define (string->number str . radix)
  (if (null? radix) (%string->number2 str 10)
      (apply %string->number2 `(,str) radix))) 

LOAD
(define (number->string num . radix)
  (if (null? radix) (%number->string2 num 10)
      (apply %number->string2 `(,num) radix))) 
