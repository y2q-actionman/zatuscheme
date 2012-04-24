#include <cstring>
#include <string>

#include "decl.hh"
#include "printer.hh"
#include "reader.hh"
#include "symtable.hh"
#include "test_util.hh"

using namespace std;

static bool result = true;

void check(SymTable& st, const char* input, const char* expect){
  static const auto callback = [input, expect](const char* buf){
    fprintf(stdout, "[failed] input:%s, expected: %s\n\treturned: %s\n",
            input, expect, buf);
  };

  Lisp_ptr p = read_from_string(st, input);

  if(!test_on_print(p, expect, callback)){
    result = false;
  }
}

void check_undef(SymTable& st, const char* input){
  Lisp_ptr p = read_from_string(st, input);

  if(p){
    fprintf(stdout, "[failed] input:%s, expected: (undefined)\n", input);
    result = false;
  }
}

int main(){
  SymTable st;

  // boolean
  check(st, "#t", "#t");
  check(st, "#f", "#f");
  check(st, "   #f", "#f");
  check(st, "#t #f", "#t");

  // char
  check(st, "#\\v", "#\\v");
  check(st, "#\\s", "#\\s");
  check(st, "#\\space", "#\\ ");

  // symbol
  check(st, "a", "a");
  check(st, "+", "+");
  check(st, "*hoge-hoge*", "*hoge-hoge*");

  // number
  check(st, "100", "100");
  check(st, "1.01", "1.01");

  // string
  check(st, "\"\"", "\"\"");
  check(st, "\"aaa aaa\"", "\"aaa aaa\"");
  check(st, "\"aa\\\\a a\\\"aa\"", "\"aa\\a a\"aa\"");
  check_undef(st, "\" \\ \"");

  // cons, list
  check(st, "()", "()");
  check(st, "(#t #f)", "(#t #f)");
  check(st, "(a b c d e)", "(a b c d e)");
  check(st, "(a () b)", "(a () b)");
  check(st, "((a b) (c d))", "((a b) (c d))");
  check(st, "(a (b . c) d e)", "(a (b . c) d e)");
  check(st, "(a (b . ()) d e)", "(a (b) d e)");
  check(st, "((((((((((a))))))))))", "((((((((((a))))))))))");
  check_undef(st, "(a . b c)");
  check_undef(st, "(. a)");
  check_undef(st, "((a)");
  check_undef(st, "(");
  check_undef(st, ")");
  check_undef(st, "#(a . b . c)");

  // vector
  check(st, "#()", "#()");
  check(st, "#(a b)", "#(a b)");
  check(st, "#(a (b c d) e)", "#(a (b c d) e)");
  check(st, "#(a (b #(c) d) e)", "#(a (b #(c) d) e)");
  check_undef(st, "#(a . b)");
  check_undef(st, "#(a . b . c)");
  check(st, "#((#((#(())))))", "#((#((#(())))))");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
