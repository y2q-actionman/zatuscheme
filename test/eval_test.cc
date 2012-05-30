#include <cstdio>
#include <cstring>

#include "eval.hh"
#include "vm.hh"
#include "builtin.hh"
#include "reader.hh"
#include "test_util.hh"

using namespace std;

static bool result = true;

template<typename Fun>
void check(const Fun& fun, const char* expr_s, const char* expect_s = nullptr){
  auto expr = read_from_string(expr_s);
  if(!expr){
    printf("reader error occured in expr!: %s\n", expr_s);
    result = false;
    return;
  }

  VM.code().push(expr);
  auto evaled = eval();
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
}

bool read_eql(Lisp_ptr input, const char* expect_s){
  auto expect = read_from_string(expect_s);
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

bool test_undef(Lisp_ptr p, const char*){
  return !p;
}

bool test_true(Lisp_ptr p, const char*){
  return !!p;
}

int main(){
  install_builtin();

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
  check(read_eql, "(if #t 1)", "1");
  check(test_undef, "(if #f 1)");
  check(read_eql, "(if #t 1 2)", "1");
  check(read_eql, "(if #f 1 2)", "2");
  check(test_undef, "(if #f 1 2 3)");

  // syntax: define
  check(read_eql, "(define x 1)", "1");
  check(read_eql, "x", "1");
  check(read_eql, "(+ x x)", "2");
  check(test_undef, "(define else 1)");
  check(read_eql, "(define else_ 1)", "1");
  check(read_eql, "else_", "1");

  // syntax: set!
  check(read_eql, "(set! x 100)", "100");
  check(read_eql, "x", "100");


  // macro call
  


  // function test (basic syntax only)
  check(test_true, "(define fun (lambda (y) (set! x y) (+ y y)))");
  check(test_true, "fun");
  check(print_equal, "(fun 2)", "4");
  check(print_equal, "x", "2");
    
  check(test_true, "(define (fun2 x) (+ 1 x))");
  check(print_equal, "(fun2 100)", "101");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
