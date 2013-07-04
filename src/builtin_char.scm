LOAD
(define (%char-cmp op)
  (lambda (c1 c2) ((force op) (char->integer c1) (char->integer c2))))

LOAD
(define char=? (%char-cmp (delay %=)))
LOAD
(define char<? (%char-cmp (delay %<)))
LOAD
(define char>? (%char-cmp (delay %>)))
LOAD
(define char<=? (%char-cmp (delay %<=)))
LOAD
(define char>=? (%char-cmp (delay %>=)))


LOAD
(define (%char-ci-cmp op)
  (lambda (c1 c2) ((force op) (%char-casecmp c1 c2) 0)))

LOAD
(define char-ci=? (%char-ci-cmp (delay %=)))
LOAD
(define char-ci<? (%char-ci-cmp (delay %<)))
LOAD
(define char-ci>? (%char-ci-cmp (delay %>)))
LOAD
(define char-ci<=? (%char-ci-cmp (delay %<=)))
LOAD
(define char-ci>=? (%char-ci-cmp (delay %>=)))
