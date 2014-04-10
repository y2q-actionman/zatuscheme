LOAD
(define eval %eval)
LOAD
(define load %load)

LOAD
(define (scheme-report-environment n)
  (if (= n 5) %r5rs-env-value))

LOAD
(define (null-environment n)
  (if (= n 5) %null-env-value))

LOAD
(define (interaction-environment)
  %interaction-env-value)
