#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}

int main(){
  install_builtin();

  check("(not #t)", "#f");
  check("(not 3)", "#f");
  check("(not (list 3))", "#f");
  check("(not #f)", "#t");
  check("(not '())", "#f");
  check("(not (list))", "#f");
  check("(not 'nil)", "#f");

  check("(boolean? #f)", "#t");
  check("(boolean? 0)", "#f");
  check("(boolean? '())", "#f");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

