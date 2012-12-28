#include "zs.hh"
#include "test_util.hh"

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}

void check_undef(const char* input){
  result &= !eval_text(input);
}

void check_success(const char* input){
  result &= !!eval_text(input);
}


int main(){
  zs_init();

  check("(vector? '#())", "#t");
  check("(vector? '#(1))", "#t");
  check("(vector? 'foo)", "#f");

  check("(make-vector 3 'a)", "#(a a a)");
  check("(make-vector 5 'a)", "#(a a a a a)");

  check("(vector 1)", "#(1)");
  check("(vector 1 2)", "#(1 2)");
  check("(vector 1 2 3)", "#(1 2 3)");
  check("(vector 'a)", "#(a)");
  check("(vector 'a 'b 'c)", "#(a b c)");
  
  check("(vector-length '#())", "0");
  check("(vector-length '#(1))", "1");
  check("(vector-length '#(1 2 3))", "3");

  check("(vector-ref '#(a) 0)", "a");
  check("(vector-ref '#(a b c d e) 2)", "c");
  with_expect_error([]() -> void {
      check_undef("(vector-ref '#(a) -1)");
      check_undef("(vector-ref '#(a) 100)");
      check_undef("(vector-ref '#() 0)");
    });
  
  check("(vector-ref '#(1 1 2 3 5 8 13 21) 5)", "8");
  check("(vector-ref '#(1 1 2 3 5 8 13 21)"
        "(let ((i (round (* 2 (acos -1)))))"
        "(if (inexact? i) (inexact->exact i) i)))",
        "13");

  
  check("(let ((vec (vector 0 '(2 2 2 2) \"Anna\"))) (vector-set! vec 1 '(\"Sue\" \"Sue\")) vec)",
        "#(0 (\"Sue\" \"Sue\") \"Anna\")");
  //check_undef("(vector-set! '#(0 1 2) 1 \"doe\")");
  with_expect_error([]() -> void {
      eval_text("(define tmpvec (vector 1))");
      check_undef("(vector-set! tmpvec -1 'hoge)");
      check_undef("(vector-set! tmpvec 100 'hoge)");
    });

  check("(vector->list '#(dah dah didah))", "(dah dah didah)");
  check("(list->vector '(dididit dah))", "#(dididit dah)");

  eval_text("(define tmpv (vector 1 2 3))");
  eval_text("(vector-fill! tmpv '?)");
  check("tmpv", "#(? ? ?)");
  eval_text("(vector-fill! tmpv '!)");
  check("tmpv", "#(! ! !)");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
