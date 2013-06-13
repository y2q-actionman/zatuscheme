LOAD
(define (map proc . lists)
  (define (all-null? lists)
    (if (null? lists) #t
        (if (null? (car lists)) (all-null? (cdr lists)) #f)))
  (define (worker lists rets)
    (if (or (null? lists) (all-null? lists))
        rets
        (let arg-collect ((args ()) (next-lists ()) (lis lists))
          (if (null? lis)
              (worker next-lists (cons (apply proc args) rets))
              (if (null? (car lis))
                  (begin (display "error_lengths_are_mismatched") (newline) #f)
                  (arg-collect (cons (caar lis) args)
                               (cons (cdar lis) next-lists)
                               (cdr lis)))))))
  (reverse (worker lists ())))

LOAD
(define (for-each proc . lists)
  (define (worker lists)
    (if (null? lists) #t
        (let arg-collect ((args ()) (next-lists ()) (lis lists))
          (if (null? lis)
              (begin (apply proc (reverse args))
                     (worker (reverse next-lists)))
              (if (null? (car lis)) #f
                  (arg-collect (cons (caar lis) args)
                               (cons (cdar lis) next-lists)
                               (cdr lis)))))))
  (worker lists))


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
