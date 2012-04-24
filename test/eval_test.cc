#include <cstdio>
#include <cstring>

#include "eval.hh"
#include "stack.hh"
#include "env.hh"
#include "builtin.hh"
#include "symtable.hh"
#include "reader.hh"
#include "test_util.hh"

using namespace std;

static bool result = true;

namespace the {
  static Env env;
  static Stack stack;
  static SymTable symtable;
}

template<typename Fun>
void check(const Fun& fun, const char* expr_s, const char* expect_s = nullptr){
  const auto start_stack_size = the::stack.size();

  auto expr = read_from_string(the::symtable, expr_s);
  if(!expr){
    printf("reader error occured in expr!: %s\n", expr_s);
    result = false;
    return;
  }

  auto evaled = eval(expr, the::env, the::stack);
  if(!evaled && expect_s){
    printf("eval error occured: %s\n", expr_s);
    result = false;
    return;
  }

  if(!fun(evaled, expect_s)){
    printf("not matched!: %s vs %s\n", expr_s, expect_s);
    result = false;
    return;
  }

  if(start_stack_size != the::stack.size()){
    printf("stack is not cleaned!!: %d -> %d (evaled: %s)\n",
           start_stack_size, the::stack.size(), expr_s);
    result = false;
    return;
  }
}

bool read_eql(Lisp_ptr input, const char* expect_s){
  auto expect = read_from_string(the::symtable, expect_s);
  if(!expect){
    printf("reader error occured in expect!: %s\n", expect_s);
    result = false;
    return false;
  }

  return eql(input, expect);
}

bool print_equal(Lisp_ptr input, const char* expect_s){
  const auto callback = [expect_s](const char* str){
    fprintf(stdout, "[failed] expected: %s\n\tevaled: %s\n",
            expect_s, str);
  };

  return test_on_print(input, expect_s, callback);
}

bool test_undef(Lisp_ptr input, const char*){
  return input.tag() == Ptr_tag::undefined;
}

int main(){
  install_builtin(the::env, the::symtable);

  // === self-evaluating ===

  check(read_eql, "#t", "#t");
  check(read_eql, "#f", "#f");

  check(read_eql, "2", "2");
  check(read_eql, "1.01", "1.01");
  check(read_eql, "1.0-3.1i", "1.0-3.1i");

  check(read_eql, "#\\R", "#\\R");
  check(read_eql, "#\\Newline", "#\\Newline");

  check(print_equal, "\"sss\"", "\"sss\"");
  check(print_equal, "\"\"", "\"\"");

  check(print_equal, "#(1 2 3)", "#(1 2 3)");
  check(print_equal, "#(1 #(11 12 13) 3)", "#(1 #(11 12 13) 3)");

  // function, port ??


  // === symbol-value ===
  check(test_undef, "tabun-tukattenai-namae");


  //check("+", "functiondkjfsa");


  // === function call ===
  check(read_eql, "(+ 1 1)", "2");


  // === Special Operator ===
  // syntax: quote
  check(test_undef, "(quote)");
  check(read_eql, "(quote 1)", "1");
  check(print_equal, "(quote (1 . 2))", "(1 . 2)");
  check(print_equal, "'(1 2 3)", "(1 2 3)");

  // syntax: lambda

  // syntax: if
  check(test_undef, "(if)");
  check(test_undef, "(if 1)");
  // check(read_eql, "(if #t 1)", "1");
  //check("(if #f 1)", "");
  // check("(if #t 1 2)", "1");
  // check("(if #f 1 2)", "2");
  //check("(if #f 1 2 3)", "");

  // syntax: set!
  // check(read_eql, "(set! x 1)", "1");
  // check("x", "1");

  // keywords are not available.
  // check(test_undef, "(set! else 1)");
  // check(read_eql, "(set! else_ 1)", "1");
  // check(test_undef, "=>");
  // check(test_undef, "define");
  // check(test_undef, "unquote");
  // check(test_undef, "unquote2");
  // check(test_undef, "unquote-splicing");
  // check(test_undef, "quote");
  // check(test_undef, "lambda");
  // check(test_undef, "if");
  // check(test_undef, "set!");
  // check(test_undef, "begin");
  // check(test_undef, "cond");
  // check(test_undef, "and");
  // check(test_undef, "or");
  // check(test_undef, "case");
  // check(test_undef, "let");
  // check(test_undef, "let*");
  // check(test_undef, "letrec");
  // check(test_undef, "do");
  // check(test_undef, "delay");
  // check(test_undef, "quasiquote");


  // macro call
  

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
