#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // === self-evaluating ===

  check_er("#t", "#t");
  check_er("#f", "#f");

  check_er("2", "2");
  check_er("1.01", "1.01");
  check_er("1.0-3.1i", "1.0-3.1i");

  check_er("#\\R", "#\\R");
  check_er("#\\Newline", "#\\Newline");

  check_e("\"sss\"", "\"sss\"");
  check_e("\"\"", "\"\"");

  check_e("#(1 2 3)", "#(1 2 3)");
  check_e("#(1 #(11 12 13) 3)", "#(1 #(11 12 13) 3)");

  check_er("()", "()");

  // function, port ??


  // === symbol-value ===
  check_e_undef("tabun-tukattenai-namae");


  // === function call ===
  check_er("(+ 1 1)", "2");


  // === Special Operator ===
  // syntax: quote
  check_e_undef("(quote)");
  check_er("(quote 1)", "1");
  check_e("(quote (1 . 2))", "(1 . 2)");
  check_e("'(1 2 3)", "(1 2 3)");

  // syntax: lambda

  // syntax: if
  check_e_undef("(if)");
  check_e_undef("(if 1)");
  check_er("(if #t 1)", "1");
  check_e_undef("(if #f 1)");
  check_er("(if #t 1 2)", "1");
  check_er("(if #f 1 2)", "2");
  check_e_undef("(if #f 1 2 3)");

  // syntax: define
  check_er("(define x 1)", "1");
  check_er("x", "1");
  check_er("(+ x x)", "2");
  //check(test_undef, "(define else 1)");
  check_er("(define else_ 1)", "1");
  check_er("else_", "1");

  // syntax: set!
  check_er("(set! x 100)", "100");
  check_er("x", "100");

  // syntax: begin
  check_e_undef("(begin)");
  check_er("(begin 1)", "1");
  check_er("(begin 1 2)", "2");
  check_er("(begin 1 2 3)", "3");

  // informal syntaxes
  //check_e_undef("else");
  check_e_undef("(else)");
  check_e_undef("(else 1)");
  //check_e_undef("=>");
  check_e_undef("(=>)");
  check_e_undef("(=> 1)");


  // macro call


  // function test (basic syntax only)
  eval_text("(define fun (lambda (y) (set! x y) (+ y y)))");
  check_e_success("fun");
  check_e("(fun 2)", "4");
  check_e("x", "2");
    
  eval_text("(define (fun2 x) (+ 1 x))");
  check_e("(fun2 100)", "101");


  check_e_undef("(fun)");
  check_e_success("(fun 191)");
  check_e_undef("(fun 100 101)");
  check_e_undef("(fun 100 101 102)");

  check_e_undef("(fun2)");
  check_e_success("(fun2 191)");
  check_e_undef("(fun2 100 101)");
  check_e_undef("(fun2 100 101 102)");
  
  return RESULT;
}
