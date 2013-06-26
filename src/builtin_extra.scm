LOAD
(define (read-eval-print-loop)
  (define read-obj #f)
  (let repl-loop ()
    (display ">> ")
    (set! read-obj (read))
    (if (eof-object? read-obj) #t
        (begin
          (call-with-current-continuation
           (lambda (c)
             (with-exception-handler
              (lambda (e) (display e) (c #f))
              (lambda () (display (eval read-obj (interaction-environment)))))))
          (newline)
          (repl-loop)))))

LOAD
(define (rsc-macro-transformer fun)
  (sc-macro-transformer
   (lambda (body usage-env)
     (capture-syntactic-environment
      (lambda (trans-env)
        (make-syntactic-closure usage-env ()
                                (fun body trans-env)))))))

LOAD
(define (close-syntax form env)
  (make-syntactic-closure env () form))

LOAD
(define (capture-syntactic-environment proc)
  `(,eval (,proc (,%current-environment)) (,%current-environment)))
