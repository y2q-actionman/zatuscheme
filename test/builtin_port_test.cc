#include "zs.hh"
#include "test_util.hh"

#define TEST_FILE_NAME "\"/tmp/zs_test.txt\""

int main(){
  zs_init();

  check_e("(input-port? 'hoge)", "#f");
  check_e("(input-port? (current-input-port))", "#t");
  check_e("(input-port? (current-output-port))", "#f");

  check_e("(output-port? 'hoge)", "#f");
  check_e("(output-port? (current-input-port))", "#f");
  check_e("(output-port? (current-output-port))", "#t");


  eval_text("(define tmpf (open-output-file "TEST_FILE_NAME"))"); 
  check_e("(input-port? tmpf)", "#f");
  check_e("(output-port? tmpf)", "#t");
  eval_text("(close-output-port tmpf)");

  eval_text("(define tmpf (open-input-file "TEST_FILE_NAME"))"); 
  check_e("(input-port? tmpf)", "#t");
  check_e("(output-port? tmpf)", "#f");
  eval_text("(close-input-port tmpf)");


  eval_text("(define tmpf (open-output-file "TEST_FILE_NAME"))"); 
  eval_text("(write-char #\\a tmpf)");
  eval_text("(write-char #\\b tmpf)");
  eval_text("(write-char #\\c tmpf)");
  eval_text("(newline tmpf)");
  eval_text("(close-output-port tmpf)");

  eval_text("(define tmpf (open-input-file "TEST_FILE_NAME"))"); 
  check_e("(peek-char tmpf)", "#\\a");
  check_e("(peek-char tmpf)", "#\\a");
  check_e("(read-char tmpf)", "#\\a");
  check_e("(peek-char tmpf)", "#\\b");
  check_e("(read-char tmpf)", "#\\b");
  check_e("(peek-char tmpf)", "#\\c");
  check_e("(read-char tmpf)", "#\\c");
  check_e("(read-char tmpf)", "#\\newline");
  check_e("(eof-object? (read-char tmpf))", "#t");
  eval_text("(close-input-port tmpf)");


  eval_text("(define tmpf (open-output-file "TEST_FILE_NAME"))"); 
  eval_text("(write '(1 2 3 4 5) tmpf)");
  eval_text("(write '#(#\\a #\\b #\\space) tmpf)");
  eval_text("(write \" \\\" \" tmpf)");
  eval_text("(close-output-port tmpf)");

  eval_text("(define tmpf (open-input-file "TEST_FILE_NAME"))"); 
  check_e("(read tmpf)", "(1 2 3 4 5)");
  check_e("(read tmpf)", "#(#\\a #\\b #\\space)");
  check_e("(read tmpf)", "\" \\\" \"");
  check_e("(eof-object? (read-char tmpf))", "#t");
  eval_text("(close-input-port tmpf)");


  eval_text("(define tmpf (open-output-file "TEST_FILE_NAME"))"); 
  eval_text("(display 1 tmpf)");
  eval_text("(display #\\a tmpf)");
  eval_text("(display \" \\\" \" tmpf)");
  eval_text("(close-output-port tmpf)");

  eval_text("(define tmpf (open-input-file "TEST_FILE_NAME"))"); 
  check_e("(read tmpf)", "1");
  check_e("(read-char tmpf)", "#\\a");
  check_e("(read-char tmpf)", "#\\space");
  check_e("(read-char tmpf)", "#\\\"");
  check_e("(read-char tmpf)", "#\\space");
  check_e("(eof-object? (read-char tmpf))", "#t");
  eval_text("(close-input-port tmpf)");


  check_e("(call-with-output-file "TEST_FILE_NAME""
          "  (lambda (port)"
          "    (write '(1 2 3 4 5) port)"
          "    (write '#(#\\a #\\b #\\space) port)"
          "    (write \" \\\" \" port)"
          "    'written))",
          "written");

  check_e("(call-with-input-file "TEST_FILE_NAME""
          "  (lambda (port)"
          "    (and (equal? (read port) '(1 2 3 4 5))"
          "         (equal? (read port) #(#\\a #\\b #\\space))"
          "         (equal? (read port) \" \\\" \"))))",
          "#t");


  check_e("(with-output-to-file "TEST_FILE_NAME""
          "  (lambda ()"
          "    (write '(1 2 3 4 5))"
          "    (write '#(#\\a #\\b #\\space))"
          "    (write \" \\\" \")"
          "    (newline)"
          "    'written))",
          "written");

  check_e("(with-input-from-file "TEST_FILE_NAME""
          "  (lambda ()"
          "    (and (equal? (read) '(1 2 3 4 5))"
          "         (equal? (read) #(#\\a #\\b #\\space))"
          "         (equal? (read) \" \\\" \"))))",
          "#t");


  return (RESULT) ? EXIT_SUCCESS : EXIT_FAILURE;
}
