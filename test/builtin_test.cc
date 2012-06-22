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

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

