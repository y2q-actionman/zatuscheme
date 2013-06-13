LOAD
(define (map proc . lists)
  (define (all-null? lists)
    (if (null? lists) %t
        (if (null? (car lists)) (all-null? (cdr lists))
            %f)))
  (define (worker lists rets)
    (if (or (null? lists) (all-null? lists))
        rets
        (let arg-collect ((args ()) (next-lists ()) (lis lists))
          (if (null? lis)
              (worker next-lists (cons (apply proc args) rets))
              (if (null? (car lis))
                  (begin (display "error_lengths_are_mismatched") (newline) %f)
                  (arg-collect (cons (caar lis) args)
                               (cons (cdar lis) next-lists)
                               (cdr lis)))))))
  (reverse (worker lists ())))

LOAD
(define (for-each proc . lists)
  (define (worker lists)
    (if (null? lists)
        %t
        (let arg-collect ((args ()) (next-lists ()) (lis lists))
          (if (null? lis)
              (begin (apply proc (reverse args))
                     (worker (reverse next-lists)))
              (if (null? (car lis))
                  %f
                  (arg-collect (cons (caar lis) args)
                               (cons (cdar lis) next-lists)
                               (cdr lis)))))))
  (worker lists))
