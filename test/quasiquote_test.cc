#include <cstdlib>
#include <cassert>

#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}


int main(){
  zs_init();

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
