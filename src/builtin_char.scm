LOAD
(define (%char-cmp op)
  (lambda (c1 c2) (op (char->integer c1) (char->integer c2))))

LOAD
(define char=? (%char-cmp %=))
LOAD
(define char<? (%char-cmp %<))
LOAD
(define char>? (%char-cmp %>))
LOAD
(define char<=? (%char-cmp %<=))
LOAD
(define char>=? (%char-cmp %>=))


LOAD
(define (%char-ci-cmp op)
  (lambda (c1 c2) (op (%char-casecmp c1 c2) 0)))

LOAD
(define char-ci=? (%char-ci-cmp %=))
LOAD
(define char-ci<? (%char-ci-cmp %<))
LOAD
(define char-ci>? (%char-ci-cmp %>))
LOAD
(define char-ci<=? (%char-ci-cmp %<=))
LOAD
(define char-ci>=? (%char-ci-cmp %>=))
