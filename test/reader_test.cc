#include <cstring>
#include <string>

#include "decl.hh"
#include "printer.hh"
#include "reader.hh"
#include "symtable.hh"
#include "test_util.hh"

using namespace std;

static bool result = true;

void check(const char* input, const char* expect){
  const auto callback = [input, expect](const char* buf){
    fprintf(stdout, "[failed] input:%s, expected: %s\n\treturned: %s\n",
            input, expect, buf);
  };

  Lisp_ptr p = read_from_string(input);

  if(!test_on_print(p, expect, callback)){
    result = false;
  }
}

void check_undef(const char* input){
  Lisp_ptr p = read_from_string(input);

  if(p){
    fprintf(stdout, "[failed] input:%s, expected: (undefined)\n", input);
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
  check("#\\space", "#\\ ");

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
  check("\"aa\\\\a a\\\"aa\"", "\"aa\\a a\"aa\"");
  check_undef("\" \\ \"");

  // cons, list
  check("()", "()");
  check("(#t #f)", "(#t #f)");
  check("(a b c d e)", "(a b c d e)");
  check("(a () b)", "(a () b)");
  check("((a b) (c d))", "((a b) (c d))");
  check("(a (b . c) d e)", "(a (b . c) d e)");
  check("(a (b . ()) d e)", "(a (b) d e)");
  check("((((((((((a))))))))))", "((((((((((a))))))))))");
  check_undef("(a . b c)");
  check_undef("(. a)");
  check_undef("((a)");
  check_undef("(");
  check_undef(")");
  check_undef("#(a . b . c)");

  // vector
  check("#()", "#()");
  check("#(a b)", "#(a b)");
  check("#(a (b c d) e)", "#(a (b c d) e)");
  check("#(a (b #(c) d) e)", "#(a (b #(c) d) e)");
  check_undef("#(a . b)");
  check_undef("#(a . b . c)");
  check("#((#((#(())))))", "#((#((#(())))))");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
