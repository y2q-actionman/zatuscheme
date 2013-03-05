#include "zs.hh"
#include "test_util.hh"

#define TEST_FILE_NAME "\"/tmp/zs_test.txt\""

int main(){
  zs_init();

  // stdin. assumes no input is supplied.
  eval_text("(let loop () (if (char-ready?) (begin (read-char) (loop)) #f))");
  check_e("(char-ready?)", "#f");

  // regular file. 
  eval_text("(define tmpf (open-output-file "TEST_FILE_NAME"))"); 
  eval_text("(write-char #\\a tmpf)");
  eval_text("(close-output-port tmpf)");

  eval_text("(define tmpf (open-input-file "TEST_FILE_NAME"))"); 
  check_e("(char-ready? tmpf)", "#t");
  check_e("(read-char tmpf)", "#\\a");

  // edge case. fd points the end-of-file.
  check_e("(char-ready? tmpf)", "#t");

  check_e("(eof-object? (read-char tmpf))", "#t");
  check_e("(char-ready? tmpf)", "#t");
  check_e("(eof-object? (read-char tmpf))", "#t");
  check_e("(char-ready? tmpf)", "#t");
  eval_text("(close-input-port tmpf)");

  return RESULT;
}
