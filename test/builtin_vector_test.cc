#include "zs.hh"
#include "test_util.hh"

void check_undef(const char* input){
  result &= !eval_text(input);
}

void check_success(const char* input){
  result &= !!eval_text(input);
}


int main(){
  zs_init();

  check_e("(vector? '#())", "#t");
  check_e("(vector? '#(1))", "#t");
  check_e("(vector? 'foo)", "#f");

  check_e("(make-vector 3 'a)", "#(a a a)");
  check_e("(make-vector 5 'a)", "#(a a a a a)");

  check_e("(vector 1)", "#(1)");
  check_e("(vector 1 2)", "#(1 2)");
  check_e("(vector 1 2 3)", "#(1 2 3)");
  check_e("(vector 'a)", "#(a)");
  check_e("(vector 'a 'b 'c)", "#(a b c)");
  
  check_e("(vector-length '#())", "0");
  check_e("(vector-length '#(1))", "1");
  check_e("(vector-length '#(1 2 3))", "3");

  check_e("(vector-ref '#(a) 0)", "a");
  check_e("(vector-ref '#(a b c d e) 2)", "c");
  with_expect_error([]() -> void {
      check_undef("(vector-ref '#(a) -1)");
      check_undef("(vector-ref '#(a) 100)");
      check_undef("(vector-ref '#() 0)");
    });
  
  check_e("(vector-ref '#(1 1 2 3 5 8 13 21) 5)", "8");
  check_e("(vector-ref '#(1 1 2 3 5 8 13 21)"
        "(let ((i (round (* 2 (acos -1)))))"
        "(if (inexact? i) (inexact->exact i) i)))",
        "13");

  
  check_e("(let ((vec (vector 0 '(2 2 2 2) \"Anna\"))) (vector-set! vec 1 '(\"Sue\" \"Sue\")) vec)",
        "#(0 (\"Sue\" \"Sue\") \"Anna\")");
  //check_undef("(vector-set! '#(0 1 2) 1 \"doe\")");
  with_expect_error([]() -> void {
      eval_text("(define tmpvec (vector 1))");
      check_undef("(vector-set! tmpvec -1 'hoge)");
      check_undef("(vector-set! tmpvec 100 'hoge)");
    });

  check_e("(vector->list '#(dah dah didah))", "(dah dah didah)");
  check_e("(list->vector '(dididit dah))", "#(dididit dah)");

  eval_text("(define tmpv (vector 1 2 3))");
  eval_text("(vector-fill! tmpv '?)");
  check_e("tmpv", "#(? ? ?)");
  eval_text("(vector-fill! tmpv '!)");
  check_e("tmpv", "#(! ! !)");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
