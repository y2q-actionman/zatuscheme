(load "zs_test_util.scm")

(test-assert (symbol? 'foo))
(test-assert (symbol? (car '(a b))))
(test-error (symbol? "bar"))
(test-assert (symbol? 'nil))
(test-error (symbol? '()))
(test-error (symbol? #f))

(test-assert (let ((ss (symbol->string 'flying-fish)))
               (or (equal? "flying-fish" ss)
                   (equal? "FLYING-FISH" ss))))

(test-assert (let ((ss (symbol->string 'Martin)))
               (or (equal? "martin" ss)
                   (equal? "MARTIN" ss))))

(test-equal "Malvina" (symbol->string (string->symbol "Malvina")))

(test-eq 'mISSISSIppi 'mississippi)

(test-assert (symbol? (string->symbol "mISSISSIppi")))

(test-error (eq? 'bitBlt (string->symbol "bitBlt")))

(test-eq 'JollyWog (string->symbol (symbol->string 'JollyWog)))

(test-assert (string=? "K. Harper, M.D."
                       (symbol->string (string->symbol "K. Harper, M.D."))))


(zs-test-report)
