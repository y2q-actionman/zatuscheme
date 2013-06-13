LOAD
(define (caar x) (car (car x)))
LOAD
(define (cadr x) (car (cdr x)))
LOAD
(define (cdar x) (cdr (car x)))
LOAD
(define (cddr x) (cdr (cdr x)))

LOAD
(define (caaar x) (car (caar x)))
LOAD
(define (caadr x) (car (cadr x)))
LOAD
(define (cadar x) (car (cdar x)))
LOAD
(define (caddr x) (car (cddr x)))
LOAD
(define (cdaar x) (cdr (caar x)))
LOAD
(define (cdadr x) (cdr (cadr x)))
LOAD
(define (cddar x) (cdr (cdar x)))
LOAD
(define (cdddr x) (cdr (cddr x)))

LOAD
(define (caaaar x) (car (caaar x)))
LOAD
(define (caaadr x) (car (caadr x)))
LOAD
(define (caadar x) (car (cadar x)))
LOAD
(define (caaddr x) (car (caddr x)))
LOAD
(define (cadaar x) (car (cdaar x)))
LOAD
(define (cadadr x) (car (cdadr x)))
LOAD
(define (caddar x) (car (cddar x)))
LOAD
(define (cadddr x) (car (cdddr x)))
LOAD
(define (cdaaar x) (cdr (caaar x)))
LOAD
(define (cdaadr x) (cdr (caadr x)))
LOAD
(define (cdadar x) (cdr (cadar x)))
LOAD
(define (cdaddr x) (cdr (caddr x)))
LOAD
(define (cddaar x) (cdr (cdaar x)))
LOAD
(define (cddadr x) (cdr (cdadr x)))
LOAD
(define (cdddar x) (cdr (cddar x)))
LOAD
(define (cddddr x) (cdr (cdddr x)))

LOAD
(define (null? x)
  (eq? x ()))

LOAD
(define (list? obj)
  (let loop ((obj obj) (conses ()))
    (cond ((null? obj) #t)
          ((not (pair? obj)) #f)
          ((memq obj conses) #f)
          (else (loop (cdr obj) (cons obj conses))))))

LOAD
(define (list . objs)
  objs)

LOAD
(define (length lis)
  (do ((l lis (cdr l))
       (n 0 (+ n 1)))
      ((null? l) n)))

LOAD
(define (append . lsts)
  (cond ((null? lsts) ())
        ((null? (cdr lsts)) (car lsts))
        ((null? (car lsts)) (apply append (cdr lsts)))
        (else
         (define head (cons (caar lsts) ()))
         (let loop ((l (cdar lsts))
                    (c head))
           (cond ((null? l)
                  (set-cdr! c (apply append (cdr lsts)))
                  head)
                 (else
                  (set-cdr! c (cons (car l) ()))
                  (loop (cdr l) (cdr c))))))))

LOAD
(define (reverse lis)
  (do ((l lis (cdr l))
       (ret () (cons (car l) ret)))
      ((null? l) ret)))

LOAD
(define (list-tail lis n)
  (if (zero? n) lis
      (list-tail (cdr lis) (- n 1))))

LOAD
(define (list-ref lis num)
  (car (list-tail lis num)))


LOAD
(define (%mem-funcs op)
  (letrec ((fun
            (lambda (obj list)
              (cond ((null? list) #f)
                    ((op obj (car list)) list)
                    (else (fun obj (cdr list)))))))
    fun))

LOAD
(define memq (%mem-funcs eq?))
LOAD
(define memv (%mem-funcs eqv?))
LOAD
(define member (%mem-funcs equal?))


LOAD
(define (%ass-funcs op)
  (letrec ((fun
            (lambda (obj alist)
              (cond ((null? alist) #f)
                    ((op obj (caar alist)) (car alist))
                    (else (fun obj (cdr alist)))))))
    fun))

LOAD
(define assq (%ass-funcs eq?))
LOAD
(define assv (%ass-funcs eqv?))
LOAD
(define assoc (%ass-funcs equal?))
