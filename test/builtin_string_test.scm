(load "zs_test_util.scm")

(test-assert (string? "foo"))
(test-error (string? 'foo))

(test-equal "aaaaaaaaaa" (make-string 10 #\a))

(test-equal "a" (string #\a))
(test-equal "abc" (string #\a #\b #\c))

(test-equal 0 (string-length ""))
(test-equal 1 (string-length "a"))
(test-equal 5 (string-length "abcde"))

(test-equal #\a (string-ref "a" 0))
(test-equal #\d (string-ref "abcde" 3))
;; (test-error (string-ref "a" -1))
;; (test-error (string-ref "a" 100))
;; (test-error (string-ref "" 0))

(define tmpstr (make-string 3 #\*))
(string-set! tmpstr 0 #\?)
(test-equal "?**" tmpstr)
(string-set! tmpstr 1 #\!)
(test-equal "?!*" tmpstr)
;; (test-error (string-set! tmpstr -1 #\_))
;; (test-error (string-set! tmpstr 100 #\_))

;; when immutable string implemented..
;; (define (f) (make-string 3 #\*))
;; (define (g) "***")
;; (test-assert (string-set! (f) 0 #\?))
;; (test-error (string-set! (g) 0 #\?))
;; (test-error (string-set! (symbol->string 'immutable) 0 #\?))

(test-assert (string=? "aaa" "aaa"))
(test-error (string=? "aaa" "aab"))
(test-error (string=? "aba" "aab"))

(test-error (string<? "aaa" "aaa"))
(test-assert (string<? "aaa" "aab"))
(test-error (string<? "aba" "aab"))

(test-error (string>? "aaa" "aaa"))
(test-error (string>? "aaa" "aab"))
(test-assert (string>? "aba" "aab"))

(test-assert (string<=? "aaa" "aaa"))
(test-assert (string<=? "aaa" "aab"))
(test-error (string<=? "aba" "aab"))

(test-assert (string>=? "aaa" "aaa"))
(test-error (string>=? "aaa" "aab"))
(test-assert (string>=? "aba" "aab"))

(test-assert (string-ci=? "Aaa" "aaa"))
(test-error (string-ci=? "Aaa" "aab"))
(test-error (string-ci=? "Aba" "aab"))

(test-error (string-ci<? "Aaa" "aaa"))
(test-assert (string-ci<? "Aaa" "aab"))
(test-error (string-ci<? "Aba" "aab"))

(test-error (string-ci>? "Aaa" "aaa"))
(test-error (string-ci>? "Aaa" "aab"))
(test-assert (string-ci>? "Aba" "aab"))

(test-assert (string-ci<=? "Aaa" "aaa"))
(test-assert (string-ci<=? "Aaa" "aab"))
(test-error (string-ci<=? "Aba" "aab"))

(test-assert (string-ci>=? "Aaa" "aaa"))
(test-error (string-ci>=? "Aaa" "aab"))
(test-assert (string-ci>=? "Aba" "aab"))


(test-equal "1234" (substring "0123456789" 1 5))
(test-equal "" (substring "0123456789" 1 1))
(test-equal "0" (substring "0123456789" 0 1))
;; (test-error (substring "0123456789" -1 1))
;; (test-error (substring "0123456789" 0 999))
;; (test-error (substring "0123456789" 5 4))
;; (test-error (substring "0123456789" 199 -1))

(test-equal "" (string-append))
(test-equal "" (string-append ""))
(test-equal "123" (string-append "" "1" "" "2" "3"))

(test-equal '() (string->list ""))
(test-equal '(#\a) (string->list "a"))
(test-equal '(#\a #\b #\c) (string->list "abc"))

(test-equal "" (list->string '()))
(test-equal "a" (list->string '(#\a)))
(test-equal "abc" (list->string '(#\a #\b #\c)))

(define str1 "abc")
(define str2 (string-copy str1))
(test-equal "abc" str2)
(string-fill! str2 #\!)
(test-equal "abc" str1)
(test-equal "!!!" str2)


(define tmpstr (string #\a #\b #\c))
(string-fill! tmpstr #\?)
(test-equal "???" tmpstr)
(string-fill! tmpstr #\!)
(test-equal "!!!" tmpstr)


(zs-test-report)
