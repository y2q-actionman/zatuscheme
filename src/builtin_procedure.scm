LOAD
(define (map proc . lists)
  (define (all-null? lists)
    (cond ((null? lists) #t)
          ((null? (car lists)) (all-null? (cdr lists)))
          (else #f)))
  (define (worker lists rets)
    (if (or (null? lists) (all-null? lists))
        rets
        (let arg-collect ((args ()) (next-lists ()) (lis lists))
          (cond ((null? lis)
                 (worker next-lists (cons (apply proc args) rets)))
                ((null? (car lis))
                 (display "error_lengths_are_mismatched") (newline) #f)
                (else
                 (arg-collect (cons (caar lis) args)
                              (cons (cdar lis) next-lists)
                              (cdr lis)))))))
  (reverse (worker lists ())))

LOAD
(define (for-each proc . lists)
  (if (null? lists) #t
      (let arg-collect ((args ()) (next-lists ()) (lis lists))
        (cond ((null? lis)
               (apply proc (reverse args))
               (apply for-each proc (reverse next-lists)))
              ((null? (car lis)) #f)
              (else
               (arg-collect (cons (caar lis) args)
                            (cons (cdar lis) next-lists)
                            (cdr lis)))))))


LOAD
(define (force object)
    (object))

LOAD
(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (make-promise (lambda () expression)))))

LOAD
(define (make-promise proc)
  (let ((result-ready? #f)
        (result #f))
    (lambda ()
      (if result-ready?
          result
          (let ((x (proc)))
            (if result-ready?
                result
                (begin (set! result-ready? #t)
                       (set! result x)
                       result)))))))
