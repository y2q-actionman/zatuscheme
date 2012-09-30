#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}

int main(){
  install_builtin();
  install_builtin_cons();

  // type predicates
  check("(pair? '(a . b))", "#t");
  check("(pair? '(a b c))", "#t");
  check("(pair? '())", "#f");
  check("(pair? #(a b))", "#f");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

