#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}

#define TEST_FILE_NAME "\"/tmp/zs_test.txt\""

int main(){
  install_builtin();

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
  check("(read-char tmpf)", "#\\\n");
  check("(eof-object? (read-char tmpf))", "#t");
  eval_text("(close-input-port tmpf)");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

