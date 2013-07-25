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
(define-syntax %prog1
  (syntax-rules ()
    ((_ x y ...)
     (let ((ret x))
       y ...
       ret))))

LOAD
(define (dynamic-wind before thunk after)
  (%push-winding before thunk after)
  (before)
  (%prog1 (thunk)
          (after)
          (%pop-winding)))
