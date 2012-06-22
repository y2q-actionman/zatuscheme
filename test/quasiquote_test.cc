#include <cstdlib>
#include <cassert>

#include "eval.hh"
#include "test_util.hh"
#include "builtin.hh"
#include "printer.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect,
                                [expect](const char* s){
                                  printf("[failed] expected %s, but got %s\n", expect, s);
                                });
}


int main(){
  install_builtin();

  check("`1", "1");
  check("`,1", "1");
  check("`#(1)", "#(1)");
  check("`()", "()");
  check("`(1)", "(1)");
  check("`(,1)", "(1)");
  
  eval_text("(define (retlist) (list 1 2 3))");
  check("(retlist)", "(1 2 3)");
  check("`(0,(retlist))", "(0 (1 2 3))");
  check("`(,@(retlist))", "(1 2 3)");

  check("`(,())", "(())");
  check("`(,() 1 2 ,() 3)", "(() 1 2 () 3)");
  check("`(,@() 1 2 ,() 3)", "(1 2 () 3)");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

