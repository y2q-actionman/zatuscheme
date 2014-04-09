LOAD
(define vector? %vector?)

LOAD
(define (make-vector k . fill)
  (apply %make-vector2 `(,k) (if (null? fill) %undefined fill)))

LOAD
(define vector %vector)

LOAD
(define vector-length %vector-length)

LOAD
(define vector-ref %vector-ref)

LOAD
(define vector-set! %vector-set!)

LOAD
(define (vector-fill! vec fill)
  (let ((size (vector-length vec)))
    (do ((i 0 (+ i 1)))
        ((= i size) vec)
      (vector-set! vec i fill))))

LOAD
(define (vector->list vec)
  (let ((size (vector-length vec)))
    (let loop ((i 0) (lis ()))
      (if (= i size) (reverse lis)
          (loop (+ i 1) (cons (vector-ref vec i) lis))))))

LOAD
(define (list->vector lis)
  (apply vector lis))
