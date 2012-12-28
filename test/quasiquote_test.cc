#include <cstdlib>

#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  check_e("`1", "1");
  check_e("`,1", "1");
  check_e("`#(1)", "#(1)");
  check_e("`()", "()");
  check_e("`(1)", "(1)");
  check_e("`(,1)", "(1)");
  
  eval_text("(define (retlist) (list 1 2 3))");
  check_e("(retlist)", "(1 2 3)");
  check_e("`(0,(retlist))", "(0 (1 2 3))");
  check_e("`(,@(retlist))", "(1 2 3)");

  check_e("`(,())", "(())");
  check_e("`(,() 1 2 ,() 3)", "(() 1 2 () 3)");
  check_e("`(,@() 1 2 ,() 3)", "(1 2 () 3)");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
