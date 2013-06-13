LOAD
(define (string . chars)
  (let* ((size (length chars))
         (str (make-string size)))
    (let loop ((i 0) (cs chars))
      (if (null? cs) str
          (begin (string-set! str i (car cs))
                 (loop (+ i 1) (cdr cs)))))))

LOAD
(define (%string-cmp op)
  (lambda (s1 s2) (op (%string-strcmp s1 s2) 0)))

LOAD
(define string=? (%string-cmp %=))
LOAD
(define string<? (%string-cmp %<))
LOAD
(define string>? (%string-cmp %>))
LOAD
(define string<=? (%string-cmp %<=))
LOAD
(define string>=? (%string-cmp %>=))

LOAD
(define (%string-ci-cmp op)
  (lambda (s1 s2) (op (%string-strcasecmp s1 s2) 0)))

LOAD
(define string-ci=? (%string-ci-cmp %=))
LOAD
(define string-ci<? (%string-ci-cmp %<))
LOAD
(define string-ci>? (%string-ci-cmp %>))
LOAD
(define string-ci<=? (%string-ci-cmp %<=))
LOAD
(define string-ci>=? (%string-ci-cmp %>=))

LOAD
(define (substring str start end)
  (let* ((size (- end start))
         (str2 (make-string size)))
    (do ((i 0 (+ i 1)))
        ((= i size) str2)
      (string-set! str2 i (string-ref str (+ i start))))))

LOAD
(define (string-append . strs)
  (let* ((size (let loop ((len 0) (s strs))
                 (if (null? s) len
                     (loop (+ len (string-length (car s))) (cdr s)))))
         (str2 (make-string size)))
    (let loop ((start 0) (s strs))
      (if (null? s) str2
          (let* ((str1 (car s))
                 (str1-len (string-length str1)))
            (do ((read-i 0 (+ read-i 1))
                 (write-i start (+ write-i 1)))
                ((= read-i str1-len) (loop write-i (cdr s)))
              (string-set! str2 write-i (string-ref str1 read-i))))))))

LOAD
(define (string->list str)
  (let ((size (string-length str)))
    (let loop ((i 0) (lis ()))
      (if (= i size) (reverse lis)
          (loop (+ i 1) (cons (string-ref str i) lis))))))

LOAD
(define (list->string lis)
  (apply string lis))

LOAD
(define (string-copy str)
  (substring str 0 (string-length str)))

LOAD
(define (string-fill! str fill)
  (let ((size (string-length str)))
    (do ((i 0 (+ i 1)))
        ((= i size) str)
      (string-set! str i fill))))
