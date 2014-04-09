LOAD
(define eval %eval)
LOAD
(define load %load)

LOAD
(define (scheme-report-environment n)
  (if (= n 5) R5RS_ENV_SYMNAME))

LOAD
(define (null-environment n)
  (if (= n 5) NULL_ENV_SYMNAME))

LOAD
(define (interaction-environment)
  INTERACTION_ENV_SYMNAME)
