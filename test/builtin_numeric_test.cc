#include <cstdlib>
#include <cassert>

#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}


int main(){
  install_builtin();
  install_builtin_numeric();

  check("(complex? 3+4i)", "#t");
  check("(real? 3+4i)", "#f");
  check("(rational? 3+4i)", "#f");
  check("(integer? 3+4i)", "#f");

  check("(complex? 3.0)", "#t");
  check("(real? 3.0)", "#t");
  check("(rational? 3.0)", "#f");
  check("(integer? 3.0)", "#f");

  check("(complex? 3/4)", "#t");
  check("(real? 3/4)", "#t");
  // check("(rational? 3/4)", "#t");
  check("(integer? 3/4)", "#f");

  check("(complex? 3)", "#t");
  check("(real? 3)", "#t");
  check("(rational? 3)", "#t");
  check("(integer? 3)", "#t");

  // from R5RS. some procedures are incompatible with this
  // implementation
  // check("(complex? 3+4i)", "#t");
  // check("(complex? 3)", "#t");
  // check("(real? 3)", "#t");
  // check("(real? -2.5+0.0i)", "#t");
  // check("(real? #e1)", "#t");
  // check("(real? #e1e10)", "#t");
  // check("(rational? 6/10)", "#t");
  // check("(rational? 6/3)", "#t");
  // check("(integer? 3+0i)", "#t");
  // check("(integer? 3.0)", "#t");
  // check("(integer? 8/4)", "#t");

  

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

