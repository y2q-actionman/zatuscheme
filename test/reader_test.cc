#include <iostream>
#include <string>

#include "zs.hh"
#include "test_util.hh"

using namespace std;

void check_undef(const char* input){
  Lisp_ptr p = read_from_string(input);

  if(p){
    cerr << "[failed] input:" << input << ", expected: (undefined)\n";
    result = false;
  }
}

int main(){
  // boolean
  check_r("#t", "#t");
  check_r("#f", "#f");
  check_r("   #f", "#f");
  check_r("#t #f", "#t");

  // char
  check_r("#\\v", "#\\v");
  check_r("#\\s", "#\\s");
  check_r("#\\space", "#\\space");

  // symbol
  check_r("a", "a");
  check_r("+", "+");
  check_r("*hoge-hoge*", "*hoge-hoge*");

  // number
  check_r("100", "100");
  check_r("1.01", "1.01");

  // string
  check_r("\"\"", "\"\"");
  check_r("\"aaa aaa\"", "\"aaa aaa\"");
  check_r("\"aa\\\\a a\\\"aa\"", "\"aa\\\\a a\\\"aa\"");
  with_expect_error([]() -> void {
      check_undef("\" \\ \"");
    });

  // cons, list
  check_r("()", "()");
  check_r("(#t #f)", "(#t #f)");
  check_r("(a b c d e)", "(a b c d e)");
  check_r("(a () b)", "(a () b)");
  check_r("((a b) (c d))", "((a b) (c d))");
  check_r("(a (b . c) d e)", "(a (b . c) d e)");
  check_r("(a (b . ()) d e)", "(a (b) d e)");
  check_r("((((((((((a))))))))))", "((((((((((a))))))))))");
  with_expect_error([]() -> void {
      check_undef("(a . b c)");
      check_undef("(. a)");
      check_undef("((a)");
      check_undef("(");
      check_undef(")");
      check_undef("#(a . b . c)");
    });

  // vector
  check_r("#()", "#()");
  check_r("#(a b)", "#(a b)");
  check_r("#(a (b c d) e)", "#(a (b c d) e)");
  check_r("#(a (b #(c) d) e)", "#(a (b #(c) d) e)");
  with_expect_error([]() -> void {
      check_undef("#(a . b)");
      check_undef("#(a . b . c)");
    });
  check_r("#((#((#(())))))", "#((#((#(())))))");

  // reader macros
  check_r("'1", "(quote 1)");
  check_r("'()", "(quote ())");
  check_r("''1", "(quote (quote 1))");
  check_r("'('1)", "(quote ((quote 1)))");

  check_r("`1", "(quasiquote 1)");
  check_r("`()", "(quasiquote ())");
  check_r("'`1", "(quote (quasiquote 1))");
  check_r("`('1)", "(quasiquote ((quote 1)))");

  check_r("`,1", "(quasiquote (unquote 1))");
  check_r("`,()", "(quasiquote (unquote ()))");
  check_r("'`1", "(quote (quasiquote 1))");
  check_r("`(,'1)", "(quasiquote ((unquote (quote 1))))");

  check_r("`(0,(+ 1 2))", "(quasiquote (0 (unquote (+ 1 2))))");
  check_r("`(0,@(+ 1 2))", "(quasiquote (0 (unquote-splicing (+ 1 2))))");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
