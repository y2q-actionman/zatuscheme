(load "zs_test_util.scm")

(test-eq #t (symbol? 'foo))
(test-eq #t (symbol? (car '(a b))))
(test-eq #f (symbol? "bar"))
(test-eq #t (symbol? 'nil))
(test-eq #f (symbol? '()))
(test-eq #f (symbol? #f))

(test-assert (let ((ss (symbol->string 'flying-fish)))
               (or (equal? "flying-fish" ss)
                   (equal? "FLYING-FISH" ss))))

(test-assert (let ((ss (symbol->string 'Martin)))
               (or (equal? "martin" ss)
                   (equal? "MARTIN" ss))))

(test-equal "Malvina" (symbol->string (string->symbol "Malvina")))

(test-eq 'mISSISSIppi 'mississippi)

(test-eq #t (symbol? (string->symbol "mISSISSIppi")))

(test-eq #f (eq? 'bitBlt (string->symbol "bitBlt")))

(test-eq 'JollyWog (string->symbol (symbol->string 'JollyWog)))

(test-eq #t (string=? "K. Harper, M.D."
                       (symbol->string (string->symbol "K. Harper, M.D."))))


(zs-test-report)
