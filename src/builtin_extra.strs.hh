// This file is intended to be included into an array of 'const char*'

"(define (read-eval-print-loop)"
"  (let loop ()"
"    (display \">> \")"
"    (display (eval (read) (interaction-environment)))"
"    (newline)"
"    (loop)))",
