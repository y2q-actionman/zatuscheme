(load "zs_test_util.scm")

;; type check
(test-assert (char? #\a))
(test-error (char? 1))

;; compare
(test-error (char=? #\0 #\1))
(test-assert (char=? #\1 #\1))
(test-error (char=? #\2 #\1))

(test-assert (char<? #\0 #\1))
(test-error (char<? #\1 #\1))
(test-error (char<? #\2 #\1))

(test-error (char>? #\0 #\1))
(test-error (char>? #\1 #\1))
(test-assert (char>? #\2 #\1))

(test-assert (char<=? #\0 #\1))
(test-assert (char<=? #\1 #\1))
(test-error (char<=? #\2 #\1))

(test-error (char>=? #\0 #\1))
(test-assert (char>=? #\1 #\1))
(test-assert (char>=? #\2 #\1))

(test-assert (char<? #\a #\b))
(test-assert (char<? #\A #\B))
(test-assert (char<? #\0 #\9))
(test-assert (char<? #\9 #\a))
(test-assert (char<? #\9 #\A))

;; case insensitive compare
(test-error (char-ci=? #\0 #\1))
(test-assert (char-ci=? #\1 #\1))
(test-error (char-ci=? #\2 #\1))

(test-assert (char-ci<? #\0 #\1))
(test-error (char-ci<? #\1 #\1))
(test-error (char-ci<? #\2 #\1))

(test-error (char-ci>? #\0 #\1))
(test-error (char-ci>? #\1 #\1))
(test-assert (char-ci>? #\2 #\1))

(test-assert (char-ci<=? #\0 #\1))
(test-assert (char-ci<=? #\1 #\1))
(test-error (char-ci<=? #\2 #\1))

(test-error (char-ci>=? #\0 #\1))
(test-assert (char-ci>=? #\1 #\1))
(test-assert (char-ci>=? #\2 #\1))

(test-assert (char-ci=? #\a #\A))
(test-error (char-ci=? #\0 #\Z))

;; char type
(test-assert (char-alphabetic? #\a))
(test-assert (char-alphabetic? #\A))
(test-error (char-alphabetic? #\0))
(test-error (char-alphabetic? #\space))

(test-error (char-numeric? #\a))
(test-error (char-numeric? #\A))
(test-assert (char-numeric? #\0))
(test-error (char-numeric? #\space))

(test-error (char-whitespace? #\a))
(test-error (char-whitespace? #\A))
(test-error (char-whitespace? #\0))
(test-assert (char-whitespace? #\space))

(test-error (char-upper-case? #\a))
(test-assert (char-upper-case? #\A))
(test-error (char-upper-case? #\0))
(test-error (char-upper-case? #\space))

(test-assert (char-lower-case? #\a))
(test-error (char-lower-case? #\A))
(test-error (char-lower-case? #\0))
(test-error (char-lower-case? #\space))

;; conversion
(test-assert (char=? #\a (integer->char (char->integer #\a))))
(test-eq 100 (char->integer (integer->char 100)))

(test-assert (char-ci=? #\a #\A))
(test-assert (char=? #\a (char-downcase #\A)))
(test-assert (char=? #\A (char-upcase #\a)))


(zs-test-report)
