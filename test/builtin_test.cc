#include <cstdlib>
#include <cassert>

#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect,
                                [expect](const char* s){
                                  printf("[failed] expected %s, but got %s\n", expect, s);
                                });
}


int main(){
  install_builtin();

  check("(list 1)", "(1)");
  check("(list 1 2)", "(1 2)");
  check("(list 1 2 3)", "(1 2 3)");

  check("(list* 1)", "1");
  check("(list* 1 2)", "(1 . 2)");
  check("(list* 1 2 3)", "(1 2 . 3)");

  check("(vector 1)", "#(1)");
  check("(vector 1 2)", "#(1 2)");
  check("(vector 1 2 3)", "#(1 2 3)");

  check("(and)", "#t");
  check("(and 1)", "1");
  check("(and 1 2)", "2");
  check("(and #f 2)", "#f");
  check("(and 1 #f 3)", "#f");
  check("(and #t #t 3)", "3");

  check("(or)", "#f");
  check("(or 1)", "1");
  check("(or #f 2)", "2");
  check("(or #f #f 3)", "3");
  check("(or 1 #f 3)", "1");
  check("(or #f 2 #f 4)", "2");

  //check("(let () 100)", "100");
  //check("(let (x) x)", "()");
  check("(let ((x 1) (y 2) (z 3)) x)", "1");
  check("(let ((x 1) (y 2) (z 3)) y)", "2");
  check("(let ((x 1) (y 2) (z 3)) z)", "3");
  check("(let ((x 1)) (let ((x 2)) x))", "2");
  check("(let ((x 1)) (let ((x 2)) x) x)", "1");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

