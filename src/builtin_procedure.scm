LOAD
(define procedure? %procedure?)

LOAD
(define apply %apply)

LOAD
(define (map proc . lists)
  (define (worker lists rets)
    (if (null? lists)
        rets
        (let arg-collect ((args ()) (next-lists ()) (lis lists))
          (cond ((null? lis)
                 (worker next-lists (cons (apply proc args) rets)))
                ((null? (car lis))
                 rets)
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
(define call-with-current-continuation %call-with-current-continuation)

LOAD
(define values %values)

LOAD
(define call-with-values %call-with-values)

LOAD
(define-syntax %multiple-value-list
  (syntax-rules ()
    ((_ form)
     (call-with-values (lambda () form) list))))

LOAD
(define-syntax %multiple-value-prog1
  (syntax-rules ()
    ((_ x y ...)
     (let ((ret (%multiple-value-list x)))
       y ...
       (apply values ret)))))

LOAD
(define (dynamic-wind before thunk after)
  (%push-winding before thunk after)
  (before)
  (%multiple-value-prog1
   (thunk)
   (after)
   (%pop-winding)))
