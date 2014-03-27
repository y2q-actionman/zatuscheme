(load "zs_test_util.scm")

;; type predicates
(test-eq #f (number? ()))
(test-eq #f (complex? ()))
(test-eq #f (real? ()))
(test-eq #f (rational? ()))
(test-eq #f (integer? ()))

(test-eq #t (number? 3+4i))
(test-eq #t (complex? 3+4i))
(test-eq #f (real? 3+4i))
(test-eq #f (rational? 3+4i))
(test-eq #f (integer? 3+4i))

(test-eq #t (number? 3.1))
(test-eq #t (complex? 3.1))
(test-eq #t (real? 3.1))
(test-eq #f (rational? 3.1))
(test-eq #f (integer? 3.1))

(test-eq #t (number? 3/4))
(test-eq #t (complex? 3/4))
(test-eq #t (real? 3/4))
(test-eq #t (rational? 3/4))
(test-eq #f (integer? 3/4))

(test-eq #t (number? 3))
(test-eq #t (complex? 3))
(test-eq #t (real? 3))
(test-eq #t (rational? 3))
(test-eq #t (integer? 3))

;; from R5RS. some procedures are incompatible with this
;; implementation
(test-eq #t (complex? 3+4i))
(test-eq #t (complex? 3))
(test-eq #t (real? 3))
(test-eq #t (real? -2.5+0.0i))
(test-eq #t (real? #e1))
(test-eq #t (real? #e1e10))
(test-eq #t (rational? 6/10))
(test-eq #t (rational? 6/3))
(test-eq #t (integer? 3+0i))
(test-eq #t (integer? 3.0))
(test-eq #t (integer? 8/4))


;; exactness
(test-eq #f (exact? ()))
(test-eq #f (inexact? ()))

(test-eq #f (exact? 3+4i))
(test-eq #t (inexact? 3+4i))
(test-eq #f (exact? 3.0))
(test-eq #t (inexact? 3.0))
(test-eq #t (exact? 3))
(test-eq #f (inexact? 3))


;; ordinate
(test-eq #t (= 1 1))
(test-eq #f (= 1 0))
(test-eq #t (= 1 1.0))   ; exact v.s. inexact
(test-eq #t (= 1 1 1 1 1 1))
(test-eq #t (= 1+1i 1+1i))
(test-eq #t (= 1 1+0i))  ; exact v.s. inexact
(test-eq #f (= 1 +1i))

(test-eq #t (> 2 1))
(test-eq #f (> 1 2))
(test-eq #f (> 1 1))
(test-eq #t (> 3 2.0 1))
(test-eq #f (> 3 2.0 1 1))
(test-eq #f (> 3 2.0 1 100))

(test-eq #t (< 1 2))
(test-eq #f (< 2 1))
(test-eq #f (< 1 1))
(test-eq #t (< 1 2.0 3))
(test-eq #f (< -1 1 1 2.0))
(test-eq #f (< 1 2 -1 6.7))

(test-eq #t (>= 2 1))
(test-eq #f (>= 1 2))
(test-eq #t (>= 1 1))
(test-eq #t (>= 3 2.0 1))
(test-eq #t (>= 3 2.0 1 1))
(test-eq #f (>= 3 2.0 1 100))

(test-eq #t (<= 1 2))
(test-eq #f (<= 2 1))
(test-eq #t (<= 1 1))
(test-eq #t (<= 1 2.0 3))
(test-eq #t (<= -1 1 1 2.0))
(test-eq #f (<= 1 2 -1 6.7))


;; property tests
(test-eq #t (zero? 0+0i))
(test-eq #f (zero? -1i))
(test-eq #f (zero? 1.0))
(test-eq #t (zero? 0.0))
(test-eq #f (zero? -1.0))
(test-eq #f (zero? 1))
(test-eq #t (zero? 0))
(test-eq #f (zero? -1))

(test-eq #f (positive? 0+0i))
(test-eq #f (positive? -1i))
(test-eq #t (positive? 1.0))
(test-eq #f (positive? 0.0))
(test-eq #f (positive? -1.0))
(test-eq #t (positive? 1))
(test-eq #f (positive? 0))
(test-eq #f (positive? -1))

(test-eq #f (negative? 0+0i))
(test-eq #f (negative? -1i))
(test-eq #f (negative? 1.0))
(test-eq #f (negative? 0.0))
(test-eq #t (negative? -1.0))
(test-eq #f (negative? 1))
(test-eq #f (negative? 0))
(test-eq #t (negative? -1))

(test-eq #f (even? 0+0i))
(test-eq #f (even? 0.0))
(test-eq #t (even? 2))
(test-eq #f (even? 1))
(test-eq #t (even? 0))
(test-eq #f (even? -1))

(test-eq #f (odd? 1+0i))
(test-eq #f (odd? 1.0))
(test-eq #f (odd? 2))
(test-eq #t (odd? 1))
(test-eq #f (odd? 0))
(test-eq #t (odd? -1))


;; min / max
(test-eqv 4 (max 1 2 3 4))
(test-approximate 4 (max -1 2.1 3 4) 0.001)
(test-eq #t (integer? (max -1 2.1 3 4)))
(test-eq #t (inexact? (max -1 2.1 3 4)))

(test-eqv 1 (min 1 2 3 4))
(test-approximate -1 (min -1 2.1 3 4) 0.001)
(test-eq #t (integer? (min -1 2.1 3 4)))
(test-eq #t (inexact? (min -1 2.1 3 4)))


;; +-*/
(test-eqv 0 (+))
(test-eqv 1 (+ 1 ))
(test-eqv 3 (+ 1 2))
(test-eqv 6 (+ 1 2 3))

(test-eqv 1 (*))
(test-eqv 1 (* 1 ))
(test-eqv 2 (* 1 2))
(test-eqv -6 (* -1 -2 -3))

(test-eqv -1 (- 1 ))
(test-eqv -1 (- 1 2))
(test-eqv -4 (- 1 2 3))

(test-eqv 1 (/ 1))
(test-eqv 1/2 (/ 1 2))
(test-eqv 1/4 (/ 1 2 2))

;; abs
(test-eqv 1 (abs 1))
(test-eqv 7 (abs -7))

;; integer division
(test-eqv 3 (quotient 13 4))
(test-eqv 1 (modulo 13 4))
(test-eqv 1 (remainder 13 4))
(test-eqv -3 (quotient -13 4))
(test-eqv 3 (modulo -13 4))
(test-eqv -1 (remainder -13 4))
(test-eqv -3 (quotient 13 -4))
(test-eqv -3 (modulo 13 -4))
(test-eqv 1 (remainder 13 -4))
(test-eqv 3 (quotient -13 -4))
(test-eqv -1 (modulo -13 -4))
(test-eqv -1 (remainder -13 -4))

;; gcd / lcm
(test-eqv 4 (gcd 32 -36))
(test-eqv 0 (gcd))
(test-eqv 288 (lcm 32 -36))
(test-eqv 1 (lcm))

;; rational
(test-eqv 3 (numerator (/ 6 4)))
(test-eqv 2 (denominator (/ 6 4)))
(test-approximate 2.0 (exact->inexact (denominator (/ 6 4))) 0.001)

;; rational arithmeric
(test-eqv 5/6 (+ 1/2 1/3))
(test-eqv 1 (+ 1/2 1/3 1/6))
(test-eqv 7/6 (+ 1/2 1/3 1/3))
(test-eqv -1/2 (- 1/2))
(test-eqv 1/6 (- 1/2 1/3))
(test-eqv 0 (- 1/2 1/3 1/6))
(test-eqv -1/6 (- 1/2 1/3 1/3))
(test-eqv 1/6 (* 1/2 1/3))
(test-eqv 1/36 (* 1/2 1/3 1/6))
(test-eqv 5/18 (* 1/2 -2/3 -5/6))
(test-eqv 2 (/ 1/2))
(test-eqv 3/2 (/ 1/2 1/3))
(test-eqv 9 (/ 1/2 1/3 1/6))
(test-eqv 9/10 (/ 1/2 -2/3 -5/6))

;; inexact
(test-approximate -5 (floor -4.3) 0.001)
(test-approximate -4 (ceiling -4.3) 0.001)
(test-approximate -4 (truncate -4.3) 0.001)
(test-approximate -4 (round -4.3) 0.001)
(test-approximate 3 (floor 3.5) 0.001)
(test-approximate 4 (ceiling 3.5) 0.001)
(test-approximate 3 (truncate 3.5) 0.001)
(test-approximate 4 (round 3.5) 0.001)
;; ;; exact
(test-eqv 7 (round 7))

;; rationalize
;; (test-approximate 1/3 (rationalize (inexact->exact .3) 1/10) 0.001)
(test-approximate #i1/3 (rationalize .3 1/10) 0.001)

;; real
;; ( (exp 2))
;; ( (log 2))
;; ( (sin 2))
;; ( (cos 2))
;; ( (tan 2))
;; ( (asin 2))
;; ( (acos 2))
;; ( (atan 2))
;; ( (atan 2 3))

(test-approximate 2 (sqrt 4) 0.001)

(test-approximate 4 (expt 2 2) 0.001)

;; complex
;; (make-rectangular x1 x2)         ===> z
;;   (make-polar x3 x4)             ===> z
;;   (real-part z)                          ===> x1
;;   (imag-part z)                          ===> x2
;;   (magnitude z)                          ===> |x3|
;;   (angle z)                              ===> xangle

;; exactness conversion
;; (exact->inexact z) 
;; (inexact->exact z) 


;; reader/writer
;; (number->string z) 
;; (number->string z radix) 
;; (let ((number number)
;;       (radix radix))
;;   (eqv? number
;;         (string->number (number->string number
;;                                         radix)
;;                         radix))) -> #t


(test-eqv 7 (string->number "7"))
(test-eqv 8 (string->number "10" 8))
(test-eqv 100 (string->number "100"))
(test-eqv 256 (string->number "100" 16))
(test-approximate 100 (string->number "1e2") 0.001)
(test-approximate 1500 (string->number "15##") 0.001)


(zs-test-report)
