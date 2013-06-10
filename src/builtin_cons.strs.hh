// This file is intended to be included into an array of 'const char*'

"(define (caar x) (car (car x)))",
"(define (cadr x) (car (cdr x)))",
"(define (cdar x) (cdr (car x)))",
"(define (cddr x) (cdr (cdr x)))",

"(define (caaar x) (car (caar x)))",
"(define (caadr x) (car (cadr x)))",
"(define (cadar x) (car (cdar x)))",
"(define (caddr x) (car (cddr x)))",
"(define (cdaar x) (cdr (caar x)))",
"(define (cdadr x) (cdr (cadr x)))",
"(define (cddar x) (cdr (cdar x)))",
"(define (cdddr x) (cdr (cddr x)))",

"(define (caaaar x) (car (caaar x)))",
"(define (caaadr x) (car (caadr x)))",
"(define (caadar x) (car (cadar x)))",
"(define (caaddr x) (car (caddr x)))",
"(define (cadaar x) (car (cdaar x)))",
"(define (cadadr x) (car (cdadr x)))",
"(define (caddar x) (car (cddar x)))",
"(define (cadddr x) (car (cdddr x)))",
"(define (cdaaar x) (cdr (caaar x)))",
"(define (cdaadr x) (cdr (caadr x)))",
"(define (cdadar x) (cdr (cadar x)))",
"(define (cdaddr x) (cdr (caddr x)))",
"(define (cddaar x) (cdr (cdaar x)))",
"(define (cddadr x) (cdr (cdadr x)))",
"(define (cdddar x) (cdr (cddar x)))",
"(define (cddddr x) (cdr (cdddr x)))",

"(define (null? x)"
"  (eq? x ()))",

"(define (list? obj)"
"  (let loop ((obj obj) (conses ()))"
"    (cond ((null? obj) #t)"
"          ((not (pair? obj)) #f)"
"          ((memq obj conses) #f)"
"          (else (loop (cdr obj) (cons obj conses))))))",

"(define (list . objs)"
"  objs)",

"(define (length lis)"
"  (do ((l lis (cdr l))"
"       (n 0 (+ n 1)))"
"      ((null? l) n)))",

"(define (reverse lis)"
"  (do ((l lis (cdr l))"
"       (ret () (cons (car l) ret)))"
"      ((null? l) ret)))",

"(define (list-tail lis n)"
"  (if (zero? n) lis"
"    (list-tail (cdr lis) (- n 1))))",

"(define (list-ref lis num)"
"  (car (list-tail lis num)))",

#define MEM_FUNCS(name, eq_op)                          \
  "(define ("name" obj list)"                           \
    "  (cond ((null? list) #f)"                         \
    "        (("eq_op" obj (car list)) list)"           \
    "        (else ("name" obj (cdr list)))))"

MEM_FUNCS("memq", "eq?"),
MEM_FUNCS("memv", "eqv?"),
MEM_FUNCS("member", "equal?"),
#undef MEM_FUNCS

#define ASS_FUNCS(name, eq_op)                          \
  "(define ("name" obj alist)"                          \
  "  (cond ((null? alist) #f)"                          \
  "        (("eq_op" obj (caar alist)) (car alist))"    \
  "        (else ("name" obj (cdr alist)))))"

ASS_FUNCS("assq", "eq?"),
ASS_FUNCS("assv", "eqv?"),
ASS_FUNCS("assoc", "equal?"),
#undef ASS_FUNCS
