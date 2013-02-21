// This file is intended to be included into an array of 'const char*'

"(define (read-eval-print-loop)"
"  (let loop ()"
"    (display \">> \")"
"    (let ((r (read)))"
"      (if (eof-object? r) #t"
"        (begin"
"          (display (eval r (interaction-environment)))"
"          (newline)"
"          (loop))))))",

"(define (rsc-macro-transformer fun)"
"  (sc-macro-transformer"
"    (lambda (body usage-env)"
"     (capture-syntactic-environment"
"      (lambda (trans-env)"
"       (make-syntactic-closure usage-env ()"
"        (fun body trans-env)))))))",

"(define (close-syntax form env)"
"  (make-syntactic-closure env () form))",
