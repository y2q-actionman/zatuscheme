// This file is intended to be included into an array of 'const char*'

"(define (string . chars)"
"  (let* ((size (length chars))"
"         (str (make-string size)))"
"    (let loop ((i 0) (cs chars))"
"       (if (null? cs) str"
"         (begin (string-set! str i (car cs))"
"                (loop (+ i 1) (cdr cs)))))))",

#define STRING_CMP_FUNCS(op)                    \
  "(define (string"op"? s1 s2)"                 \
  "  ("op" (%string-strcmp s1 s2) 0))"
STRING_CMP_FUNCS("="),
STRING_CMP_FUNCS("<"),
STRING_CMP_FUNCS(">"),
STRING_CMP_FUNCS("<="),
STRING_CMP_FUNCS(">="),
#undef STRING_CMP_FUNCS

#define STRING_CI_CMP_FUNCS(op)                    \
  "(define (string-ci"op"? s1 s2)"                 \
  "  ("op" (%string-strcasecmp s1 s2) 0))"
STRING_CI_CMP_FUNCS("="),
STRING_CI_CMP_FUNCS("<"),
STRING_CI_CMP_FUNCS(">"),
STRING_CI_CMP_FUNCS("<="),
STRING_CI_CMP_FUNCS(">="),
#undef STRING_CI_CMP_FUNCS

"(define (substring str start end)"
"  (let* ((size (- end start))"
"         (str2 (make-string size)))"
"    (do ((i 0 (+ i 1)))"
"        ((= i size) str2)"
"      (string-set! str2 i (string-ref str (+ i start))))))",

"(define (string->list str)"
"  (let ((size (string-length str)))"
"    (let loop ((i 0) (lis ()))"
"       (if (= i size) (reverse lis)"
"         (loop (+ i 1) (cons (string-ref str i) lis))))))",

"(define (list->string lis)"
"  (apply string lis))",

"(define (string-copy str)"
"  (substring str 0 (string-length str)))",

"(define (string-fill! str fill)"
"  (let ((size (string-length str)))"
"    (do ((i 0 (+ i 1)))"
"        ((= i size) str)"
"      (string-set! str i fill))))",
