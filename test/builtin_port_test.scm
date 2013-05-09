(load "zs_test_util.scm")

(define test-file-name "/tmp/zs_test.txt")

(test-error (input-port? 'hoge))
(test-assert (input-port? (current-input-port)))
(test-error (input-port? (current-output-port)))

(test-error (output-port? 'hoge))
(test-error (output-port? (current-input-port)))
(test-assert (output-port? (current-output-port)))

(define tmpf (open-output-file test-file-name)) 
(test-error (input-port? tmpf))
(test-assert (output-port? tmpf))
(close-output-port tmpf)

(define tmpf (open-input-file test-file-name)) 
(test-assert (input-port? tmpf))
(test-error (output-port? tmpf))
(close-input-port tmpf)

(define tmpf (open-output-file test-file-name)) 
(write-char #\a tmpf)
(write-char #\b tmpf)
(write-char #\c tmpf)
(newline tmpf)
(close-output-port tmpf)

(define tmpf (open-input-file test-file-name)) 
(test-eqv #\a (peek-char tmpf))
(test-eqv #\a (peek-char tmpf))
(test-eqv #\a (read-char tmpf))
(test-eqv #\b (peek-char tmpf))
(test-eqv #\b (read-char tmpf))
(test-eqv #\c (peek-char tmpf))
(test-eqv #\c (read-char tmpf))
(test-eqv #\newline (read-char tmpf))
(test-assert (eof-object? (read-char tmpf)))
(close-input-port tmpf)

(define tmpf (open-output-file test-file-name)) 
(write '(1 2 3 4 5) tmpf)
(write '#(#\a #\b #\space) tmpf)
(write " \" " tmpf)
(close-output-port tmpf)

(define tmpf (open-input-file test-file-name)) 
(test-equal '(1 2 3 4 5) (read tmpf))
(test-equal #(#\a #\b #\space) (read tmpf))
(test-equal " \" " (read tmpf))
(test-assert (eof-object? (read-char tmpf)))
(close-input-port tmpf)


(define tmpf (open-output-file test-file-name)) 
(display 1 tmpf)
(display #\a tmpf)
(display " \" " tmpf)
(close-output-port tmpf)

(define tmpf (open-input-file test-file-name)) 
(test-eqv 1 (read tmpf))
(test-eqv #\a (read-char tmpf))
(test-eqv #\space (read-char tmpf))
(test-eqv #\" (read-char tmpf))
(test-eqv #\space (read-char tmpf))
(test-assert (eof-object? (read-char tmpf)))
(close-input-port tmpf)

(test-eqv 'written
          (call-with-output-file test-file-name
            (lambda (port)
              (write '(1 2 3 4 5) port)
              (write '#(#\a #\b #\space) port)
              (write " \" " port)
              'written)))

(test-assert
 (call-with-input-file test-file-name
   (lambda (port)
     (and (equal? (read port) '(1 2 3 4 5))
          (equal? (read port) #(#\a #\b #\space))
          (equal? (read port) " \" ")))))


(test-eqv 'written
          (with-output-to-file test-file-name
            (lambda ()
              (write '(1 2 3 4 5))
              (write '#(#\a #\b #\space))
              (write " \" ")
              (newline)
              'written)))

(test-assert
 (with-input-from-file test-file-name
   (lambda ()
     (and (equal? (read) '(1 2 3 4 5))
          (equal? (read) #(#\a #\b #\space))
          (equal? (read) " \" ")))))


(zs-test-report)
