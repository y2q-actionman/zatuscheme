#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}

#define TEST_FILE_NAME "\"/tmp/zs_test.txt\""

int main(){
  zs_init();

  check("(input-port? 'hoge)", "#f");
  check("(input-port? (current-input-port))", "#t");
  check("(input-port? (current-output-port))", "#f");

  check("(output-port? 'hoge)", "#f");
  check("(output-port? (current-input-port))", "#f");
  check("(output-port? (current-output-port))", "#t");


  eval_text("(define tmpf (open-output-file "TEST_FILE_NAME"))"); 
  check("(input-port? tmpf)", "#f");
  check("(output-port? tmpf)", "#t");
  eval_text("(close-output-port tmpf)");

  eval_text("(define tmpf (open-input-file "TEST_FILE_NAME"))"); 
  check("(input-port? tmpf)", "#t");
  check("(output-port? tmpf)", "#f");
  eval_text("(close-input-port tmpf)");


  eval_text("(define tmpf (open-output-file "TEST_FILE_NAME"))"); 
  eval_text("(write-char #\\a tmpf)");
  eval_text("(write-char #\\b tmpf)");
  eval_text("(write-char #\\c tmpf)");
  eval_text("(newline tmpf)");
  eval_text("(close-output-port tmpf)");

  eval_text("(define tmpf (open-input-file "TEST_FILE_NAME"))"); 
  check("(peek-char tmpf)", "#\\a");
  check("(peek-char tmpf)", "#\\a");
  check("(read-char tmpf)", "#\\a");
  check("(peek-char tmpf)", "#\\b");
  check("(read-char tmpf)", "#\\b");
  check("(peek-char tmpf)", "#\\c");
  check("(read-char tmpf)", "#\\c");
  check("(read-char tmpf)", "#\\newline");
  check("(eof-object? (read-char tmpf))", "#t");
  eval_text("(close-input-port tmpf)");


  eval_text("(define tmpf (open-output-file "TEST_FILE_NAME"))"); 
  eval_text("(write '(1 2 3 4 5) tmpf)");
  eval_text("(write '#(#\\a #\\b #\\space) tmpf)");
  eval_text("(write \" \\\" \" tmpf)");
  eval_text("(close-output-port tmpf)");

  eval_text("(define tmpf (open-input-file "TEST_FILE_NAME"))"); 
  check("(read tmpf)", "(1 2 3 4 5)");
  check("(read tmpf)", "#(#\\a #\\b #\\space)");
  check("(read tmpf)", "\" \\\" \"");
  check("(eof-object? (read-char tmpf))", "#t");
  eval_text("(close-input-port tmpf)");


  eval_text("(define tmpf (open-output-file "TEST_FILE_NAME"))"); 
  eval_text("(display 1 tmpf)");
  eval_text("(display #\\a tmpf)");
  eval_text("(display \" \\\" \" tmpf)");
  eval_text("(close-output-port tmpf)");

  eval_text("(define tmpf (open-input-file "TEST_FILE_NAME"))"); 
  check("(read tmpf)", "1");
  check("(read-char tmpf)", "#\\a");
  check("(read-char tmpf)", "#\\space");
  check("(read-char tmpf)", "#\\\"");
  check("(read-char tmpf)", "#\\space");
  check("(eof-object? (read-char tmpf))", "#t");
  eval_text("(close-input-port tmpf)");


  check("(call-with-output-file "TEST_FILE_NAME""
        "  (lambda (port)"
        "    (write '(1 2 3 4 5) port)"
        "    (write '#(#\\a #\\b #\\space) port)"
        "    (write \" \\\" \" port)"
        "    'written))",
        "written");

  check("(call-with-input-file "TEST_FILE_NAME""
        "  (lambda (port)"
        "    (and (equal? (read port) '(1 2 3 4 5))"
        "         (equal? (read port) #(#\\a #\\b #\\space))"
        "         (equal? (read port) \" \\\" \"))))",
        "#t");


  check("(with-output-to-file "TEST_FILE_NAME""
        "  (lambda ()"
        "    (write '(1 2 3 4 5))"
        "    (write '#(#\\a #\\b #\\space))"
        "    (write \" \\\" \")"
        "    (newline)"
        "    'written))",
        "written");

  check("(with-input-from-file "TEST_FILE_NAME""
        "  (lambda ()"
        "    (and (equal? (read) '(1 2 3 4 5))"
        "         (equal? (read) #(#\\a #\\b #\\space))"
        "         (equal? (read) \" \\\" \"))))",
        "#t");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
