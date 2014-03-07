LOAD
(define (scheme-report-environment n)
  (if (= n 5) R5RS_ENV_SYMNAME))

LOAD
(define (null-environment n)
  (if (= n 5) NULL_ENV_SYMNAME))

LOAD
(define (interaction-environment)
  INTERACTION_ENV_SYMNAME)

LOAD
(define (load filename)
  "This code uses interaction-environment implicitly"
  (call-with-input-file filename
    (lambda (s)
      (let loop ((form (read s)))
	(if (eof-object? form)
	    form
	    (begin
	      (eval form (interaction-environment))
	      (loop (read s))))))))
