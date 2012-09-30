#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}

int main(){
  install_builtin();
  install_builtin_cons();

  check("(pair? '(a . b))", "#t");
  check("(pair? '(a b c))", "#t");
  check("(pair? '())", "#f");
  check("(pair? #(a b))", "#f");

  check("(cons 'a '())", "(a)");
  check("(cons '(a) '(b c d))", "((a) b c d)");
  check("(cons \"a\" '(b c))", "(\"a\" b c)");
  check("(cons 'a 3)", "(a . 3)");
  check("(cons '(a  b) 'c)", "((a b) . c)");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

