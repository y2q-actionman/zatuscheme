(load "zs_test_util.scm")

;; type check
(test-eq #t (char? #\a))
(test-eq #f (char? 1))

;; compare
(test-eq #f (char=? #\0 #\1))
(test-eq #t (char=? #\1 #\1))
(test-eq #f (char=? #\2 #\1))

(test-eq #t (char<? #\0 #\1))
(test-eq #f (char<? #\1 #\1))
(test-eq #f (char<? #\2 #\1))

(test-eq #f (char>? #\0 #\1))
(test-eq #f (char>? #\1 #\1))
(test-eq #t (char>? #\2 #\1))

(test-eq #t (char<=? #\0 #\1))
(test-eq #t (char<=? #\1 #\1))
(test-eq #f (char<=? #\2 #\1))

(test-eq #f (char>=? #\0 #\1))
(test-eq #t (char>=? #\1 #\1))
(test-eq #t (char>=? #\2 #\1))

(test-eq #t (char<? #\a #\b))
(test-eq #t (char<? #\A #\B))
(test-eq #t (char<? #\0 #\9))
(test-eq #t (char<? #\9 #\a))
(test-eq #t (char<? #\9 #\A))

;; case insensitive compare
(test-eq #f (char-ci=? #\0 #\1))
(test-eq #t (char-ci=? #\1 #\1))
(test-eq #f (char-ci=? #\2 #\1))

(test-eq #t (char-ci<? #\0 #\1))
(test-eq #f (char-ci<? #\1 #\1))
(test-eq #f (char-ci<? #\2 #\1))

(test-eq #f (char-ci>? #\0 #\1))
(test-eq #f (char-ci>? #\1 #\1))
(test-eq #t (char-ci>? #\2 #\1))

(test-eq #t (char-ci<=? #\0 #\1))
(test-eq #t (char-ci<=? #\1 #\1))
(test-eq #f (char-ci<=? #\2 #\1))

(test-eq #f (char-ci>=? #\0 #\1))
(test-eq #t (char-ci>=? #\1 #\1))
(test-eq #t (char-ci>=? #\2 #\1))

(test-eq #t (char-ci=? #\a #\A))
(test-eq #f (char-ci=? #\0 #\Z))

;; char type
(test-eq #t (char-alphabetic? #\a))
(test-eq #t (char-alphabetic? #\A))
(test-eq #f (char-alphabetic? #\0))
(test-eq #f (char-alphabetic? #\space))

(test-eq #f (char-numeric? #\a))
(test-eq #f (char-numeric? #\A))
(test-eq #t (char-numeric? #\0))
(test-eq #f (char-numeric? #\space))

(test-eq #f (char-whitespace? #\a))
(test-eq #f (char-whitespace? #\A))
(test-eq #f (char-whitespace? #\0))
(test-eq #t (char-whitespace? #\space))

(test-eq #f (char-upper-case? #\a))
(test-eq #t (char-upper-case? #\A))
(test-eq #f (char-upper-case? #\0))
(test-eq #f (char-upper-case? #\space))

(test-eq #t (char-lower-case? #\a))
(test-eq #f (char-lower-case? #\A))
(test-eq #f (char-lower-case? #\0))
(test-eq #f (char-lower-case? #\space))

;; conversion
(test-eq #t (char=? #\a (integer->char (char->integer #\a))))
(test-eq 100 (char->integer (integer->char 100)))

(test-eq #t (char-ci=? #\a #\A))
(test-eq #t (char=? #\a (char-downcase #\A)))
(test-eq #t (char=? #\A (char-upcase #\a)))


(zs-test-report)
