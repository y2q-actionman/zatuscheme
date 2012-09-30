#include <cstdlib>

#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}

int main(){
  install_builtin();
  install_builtin_numeric();

  // type predicates
  check("(complex? ())", "#f");
  check("(real? ())", "#f");
  check("(rational? ())", "#f");
  check("(integer? ())", "#f");

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
  check("(complex? 3+4i)", "#t");
  // check("(complex? 3)", "#t");
  check("(real? 3)", "#t");
  // check("(real? -2.5+0.0i)", "#t");
  check("(real? #e1)", "#t");
  check("(real? #e1e10)", "#t");
  // check("(rational? 6/10)", "#t");
  // check("(rational? 6/3)", "#t");
  // check("(integer? 3+0i)", "#t");
  // check("(integer? 3.0)", "#t");
  // check("(integer? 8/4)", "#t");

  check("(exact? ())", "#f");
  check("(inexact? ())", "#f");

  check("(exact? 3+4i)", "#f");
  check("(inexact? 3+4i)", "#t");
  check("(exact? 3.0)", "#f");
  check("(inexact? 3.0)", "#t");
  check("(exact? 3)", "#t");
  check("(inexact? 3)", "#f");


  check("(= 1 1)", "#t");
  check("(= 1 0)", "#f");
  check("(= 1 1.0)", "#t");
  check("(= 1 1 1 1 1 1)", "#t");
  check("(= 1+1i 1+1i)", "#t");
  check("(= 1 1+0i)", "#t");

  check("(> 2 1)", "#t");
  check("(> 1 2)", "#f");
  check("(> 1 1)", "#f");
  check("(> 3 2.0 1)", "#t");
  check("(> 3 2.0 1 1)", "#f");
  check("(> 3 2.0 1 100)", "#f");

  check("(< 1 2)", "#t");
  check("(< 2 1)", "#f");
  check("(< 1 1)", "#f");
  check("(< 1 2.0 3)", "#t");
  check("(< -1 1 1 2.0)", "#f");
  check("(< 1 2 -1 6.7)", "#f");

  check("(>= 2 1)", "#t");
  check("(>= 1 2)", "#f");
  check("(>= 1 1)", "#t");
  check("(>= 3 2.0 1)", "#t");
  check("(>= 3 2.0 1 1)", "#t");
  check("(>= 3 2.0 1 100)", "#f");

  check("(<= 1 2)", "#t");
  check("(<= 2 1)", "#f");
  check("(<= 1 1)", "#t");
  check("(<= 1 2.0 3)", "#t");
  check("(<= -1 1 1 2.0)", "#t");
  check("(<= 1 2 -1 6.7)", "#f");


  check("(zero? 0+0i)", "#t");
  check("(zero? -1i)", "#f");
  check("(zero? 1.0)", "#f");
  check("(zero? 0.0)", "#t");
  check("(zero? -1.0)", "#f");
  check("(zero? 1)", "#f");
  check("(zero? 0)", "#t");
  check("(zero? -1)", "#f");

  check("(positive? 0+0i)", "#f");
  check("(positive? -1i)", "#f");
  check("(positive? 1.0)", "#t");
  check("(positive? 0.0)", "#f");
  check("(positive? -1.0)", "#f");
  check("(positive? 1)", "#t");
  check("(positive? 0)", "#f");
  check("(positive? -1)", "#f");

  check("(negative? 0+0i)", "#f");
  check("(negative? -1i)", "#f");
  check("(negative? 1.0)", "#f");
  check("(negative? 0.0)", "#f");
  check("(negative? -1.0)", "#t");
  check("(negative? 1)", "#f");
  check("(negative? 0)", "#f");
  check("(negative? -1)", "#t");

  check("(even? 0+0i)", "#f");
  check("(even? 0.0)", "#f");
  check("(even? 2)", "#t");
  check("(even? 1)", "#f");
  check("(even? 0)", "#t");
  check("(even? -1)", "#f");

  check("(odd? 1+0i)", "#f");
  check("(odd? 1.0)", "#f");
  check("(odd? 2)", "#f");
  check("(odd? 1)", "#t");
  check("(odd? 0)", "#f");
  check("(odd? -1)", "#t");


  check("(max 1 2 3 4)", "4");
  check("(max -1 2.1 3 4)", "4");
  check("(integer? (max -1 2.1 3 4))", "#f");

  check("(min 1 2 3 4)", "1");
  check("(min -1 2.1 3 4)", "-1");
  check("(integer? (min -1 2.1 3 4))", "#f");


  check("(+)", "0");
  check("(+ 1 )", "1");
  check("(+ 1 2)", "3");
  check("(+ 1 2 3)", "6");

  check("(*)", "1");
  check("(* 1 )", "1");
  check("(* 1 2)", "2");
  check("(* -1 -2 -3)", "-6");

  check("(- 1 )", "-1");
  check("(- 1 2)", "-1");
  check("(- 1 2 3)", "-4");

  check("(/ 1)", "1");
  check("(/ 1 2)", "0.5");
  check("(/ 1 2 2)", "0.25");


  check("(abs 1)", "1");
  check("(abs -7)", "7");

  check("(modulo 13 4)", "1");
  check("(remainder 13 4)", "1");
  check("(modulo -13 4)", "3");
  check("(remainder -13 4)", "-1");
  check("(modulo 13 -4)", "-3");
  check("(remainder 13 -4)", "1");
  check("(modulo -13 -4)", "-1");
  check("(remainder -13 -4)", "-1");

  check("(gcd 32 -36)", "4");
  check("(gcd)", "0");
  check("(lcm 32 -36)", "288");
  check("(lcm)", "1");

  // inexact
  check("(floor -4.3)", "-5");
  check("(ceiling -4.3)", "-4");
  check("(truncate -4.3)", "-4");
  check("(round -4.3)", "-4");
  check("(floor 3.5)", "3");
  check("(ceiling 3.5)", "4");
  check("(truncate 3.5)", "3");
  check("(round 3.5)", "4");
  // exact
  check("(round 7)", "7");

  // reader/writer
  check("(string->number \"7\")", "7");
  check("(string->number \"10\" 8)", "8");
  check("(string->number \"100\")", "100");
  check("(string->number \"100\" 16)", "256");
  check("(string->number \"1e2\")", "100");
  check("(string->number \"15##\")", "1500");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

