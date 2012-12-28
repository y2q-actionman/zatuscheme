#include "zs.hh"
#include "test_util.hh"

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
  check_r_undef("\" \\ \"");

  // cons, list
  check_r("()", "()");
  check_r("(#t #f)", "(#t #f)");
  check_r("(a b c d e)", "(a b c d e)");
  check_r("(a () b)", "(a () b)");
  check_r("((a b) (c d))", "((a b) (c d))");
  check_r("(a (b . c) d e)", "(a (b . c) d e)");
  check_r("(a (b . ()) d e)", "(a (b) d e)");
  check_r("((((((((((a))))))))))", "((((((((((a))))))))))");
  check_r_undef("(a . b c)");
  check_r_undef("(. a)");
  check_r_undef("((a)");
  check_r_undef("(");
  check_r_undef(")");
  check_r_undef("#(a . b . c)");

  // vector
  check_r("#()", "#()");
  check_r("#(a b)", "#(a b)");
  check_r("#(a (b c d) e)", "#(a (b c d) e)");
  check_r("#(a (b #(c) d) e)", "#(a (b #(c) d) e)");
  check_r_undef("#(a . b)");
  check_r_undef("#(a . b . c)");
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

  return RESULT;
}
