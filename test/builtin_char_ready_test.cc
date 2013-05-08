#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // stdin. assumes no input is supplied.
  eval_text("(let loop () (if (char-ready?) (begin (read-char) (loop)) #f))");
  check_e("(char-ready?)", "#f");

  // regular file. 
  eval_text("(define tmpf-in #f)");
  eval_text("(define tmpf-out #f)");
  eval_text("(let ((ports (tmp-file))) (set! tmpf-in (car ports)) (set! tmpf-out (cadr ports)))");

  eval_text("(write-char #\\a tmpf-out)");
  eval_text("(close-output-port tmpf-out)");

  check_e("(char-ready? tmpf-in)", "#t");
  check_e("(read-char tmpf-in)", "#\\a");

  // edge case. fd points the end-of-file, just.
  // check_e("(char-ready? tmpf-in)", "#t");

  check_e("(eof-object? (read-char tmpf-in))", "#t");
  check_e("(char-ready? tmpf-in)", "#t");
  check_e("(eof-object? (read-char tmpf-in))", "#t");
  check_e("(char-ready? tmpf-in)", "#t");
  eval_text("(close-input-port tmpf-in)");

  return RESULT;
}
