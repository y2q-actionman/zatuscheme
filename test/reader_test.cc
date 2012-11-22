#include <iostream>
#include <string>

#include "zs.hh"
#include "test_util.hh"

using namespace std;

static bool result = true;

void check(const char* input, const char* expect){
  const auto callback = [input, expect](const char* buf){
    cerr << "[failed] input:" << input << ", expected: " << expect << "\n"
         << "\treturned: " << buf << "\n";
  };

  Lisp_ptr p = read_from_string(input);

  if(!test_on_print(p, expect, callback)){
    result = false;
  }
}

void check_undef(const char* input){
  Lisp_ptr p = read_from_string(input);

  if(p){
    cerr << "[failed] input:" << input << ", expected: (undefined)\n";
    result = false;
  }
}

int main(){
  // boolean
  check("#t", "#t");
  check("#f", "#f");
  check("   #f", "#f");
  check("#t #f", "#t");

  // char
  check("#\\v", "#\\v");
  check("#\\s", "#\\s");
  check("#\\space", "#\\space");

  // symbol
  check("a", "a");
  check("+", "+");
  check("*hoge-hoge*", "*hoge-hoge*");

  // number
  check("100", "100");
  check("1.01", "1.01");

  // string
  check("\"\"", "\"\"");
  check("\"aaa aaa\"", "\"aaa aaa\"");
  check("\"aa\\\\a a\\\"aa\"", "\"aa\\\\a a\\\"aa\"");
  with_expect_error([]() -> void {
      check_undef("\" \\ \"");
    });

  // cons, list
  check("()", "()");
  check("(#t #f)", "(#t #f)");
  check("(a b c d e)", "(a b c d e)");
  check("(a () b)", "(a () b)");
  check("((a b) (c d))", "((a b) (c d))");
  check("(a (b . c) d e)", "(a (b . c) d e)");
  check("(a (b . ()) d e)", "(a (b) d e)");
  check("((((((((((a))))))))))", "((((((((((a))))))))))");
  with_expect_error([]() -> void {
      check_undef("(a . b c)");
      check_undef("(. a)");
      check_undef("((a)");
      check_undef("(");
      check_undef(")");
      check_undef("#(a . b . c)");
    });

  // vector
  check("#()", "#()");
  check("#(a b)", "#(a b)");
  check("#(a (b c d) e)", "#(a (b c d) e)");
  check("#(a (b #(c) d) e)", "#(a (b #(c) d) e)");
  with_expect_error([]() -> void {
      check_undef("#(a . b)");
      check_undef("#(a . b . c)");
    });
  check("#((#((#(())))))", "#((#((#(())))))");

  // reader macros
  check("'1", "(quote 1)");
  check("'()", "(quote ())");
  check("''1", "(quote (quote 1))");
  check("'('1)", "(quote ((quote 1)))");

  check("`1", "(quasiquote 1)");
  check("`()", "(quasiquote ())");
  check("'`1", "(quote (quasiquote 1))");
  check("`('1)", "(quasiquote ((quote 1)))");

  check("`,1", "(quasiquote (unquote 1))");
  check("`,()", "(quasiquote (unquote ()))");
  check("'`1", "(quote (quasiquote 1))");
  check("`(,'1)", "(quasiquote ((unquote (quote 1))))");

  check("`(0,(+ 1 2))", "(quasiquote (0 (unquote (+ 1 2))))");
  check("`(0,@(+ 1 2))", "(quasiquote (0 (unquote-splicing (+ 1 2))))");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
