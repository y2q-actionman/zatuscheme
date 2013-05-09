(load "zs_test_util.scm")

;; testing simple closure
(define x 1)
(define (hoge) x)
(test-equal 1 (hoge))

(define x 100)
(test-equal 100 x)
(test-equal 100 (hoge))

(define fuga
  (lambda (x) (lambda () x)))

(define fun123 (fuga 123))
(test-equal 123 (fun123))

(define fun256 (fuga 256))
(test-equal 256 (fun256))

(define x 100)
(test-equal 100 x)
(test-equal 100 (hoge))
(test-equal 123 (fun123))
(test-equal 256 (fun256))

(define (fun2 x) (+ x 1))
(test-equal 101 (fun2 100))

;; testing defined variables
(define (hoge)
  (define (fuga n)
    (* n 2))
  fuga)
(test-equal 200 ((hoge) 100))

;; testing various syntaxes


(zs-test-report)
