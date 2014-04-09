LOAD
(define char? %char?)

LOAD
(define (%char-cmp op)
  (lambda (c1 c2) (op (%char->integer c1) (%char->integer c2))))
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

LOAD
(define char-alphabetic? %char-alphabetic?)
LOAD
(define char-numeric? %char-numeric?)
LOAD
(define char-whitespace? %char-whitespace?)
LOAD
(define char-upper-case? %char-upper-case?)
LOAD
(define char-lower-case? %char-lower-case?)

LOAD
(define char->integer %char->integer)
LOAD
(define integer->char %integer->char)

LOAD
(define char-upcase %char-upcase)
LOAD
(define char-downcase %char-downcase)
