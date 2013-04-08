#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // type predicates
  check_e("(number? ())", "#f");
  check_e("(complex? ())", "#f");
  check_e("(real? ())", "#f");
  check_e("(rational? ())", "#f");
  check_e("(integer? ())", "#f");

  check_e("(number? 3+4i)", "#t");
  check_e("(complex? 3+4i)", "#t");
  check_e("(real? 3+4i)", "#f");
  check_e("(rational? 3+4i)", "#f");
  check_e("(integer? 3+4i)", "#f");

  check_e("(number? 3.0)", "#t");
  check_e("(complex? 3.0)", "#t");
  check_e("(real? 3.0)", "#t");
  check_e("(rational? 3.0)", "#f");
  check_e("(integer? 3.0)", "#f");

  check_e("(number? 3/4)", "#t");
  check_e("(complex? 3/4)", "#t");
  check_e("(real? 3/4)", "#t");
  // check_e("(rational? 3/4)", "#t");
  check_e("(integer? 3/4)", "#f");

  check_e("(number? 3)", "#t");
  check_e("(complex? 3)", "#t");
  check_e("(real? 3)", "#t");
  check_e("(rational? 3)", "#t");
  check_e("(integer? 3)", "#t");

  // from R5RS. some procedures are incompatible with this
  // implementation
  check_e("(complex? 3+4i)", "#t");
  check_e("(complex? 3)", "#t");
  check_e("(real? 3)", "#t");
  // check_e("(real? -2.5+0.0i)", "#t");
  check_e("(real? #e1)", "#t");
  check_e("(real? #e1e10)", "#t");
  // check_e("(rational? 6/10)", "#t");
  // check_e("(rational? 6/3)", "#t");
  // check_e("(integer? 3+0i)", "#t");
  // check_e("(integer? 3.0)", "#t");
  // check_e("(integer? 8/4)", "#t");


  // exactness
  check_e("(exact? ())", "#f");
  check_e("(inexact? ())", "#f");

  check_e("(exact? 3+4i)", "#f");
  check_e("(inexact? 3+4i)", "#t");
  check_e("(exact? 3.0)", "#f");
  check_e("(inexact? 3.0)", "#t");
  check_e("(exact? 3)", "#t");
  check_e("(inexact? 3)", "#f");


  // ordinate
  check_e("(= 1 1)", "#t");
  check_e("(= 1 0)", "#f");
  check_e("(= 1 1.0)", "#t");   // exact v.s. inexact
  check_e("(= 1 1 1 1 1 1)", "#t");
  check_e("(= 1+1i 1+1i)", "#t");
  check_e("(= 1 1+0i)", "#t");  // exact v.s. inexact
  check_e("(= 1 +1i)", "#f");

  check_e("(> 2 1)", "#t");
  check_e("(> 1 2)", "#f");
  check_e("(> 1 1)", "#f");
  check_e("(> 3 2.0 1)", "#t");
  check_e("(> 3 2.0 1 1)", "#f");
  check_e("(> 3 2.0 1 100)", "#f");

  check_e("(< 1 2)", "#t");
  check_e("(< 2 1)", "#f");
  check_e("(< 1 1)", "#f");
  check_e("(< 1 2.0 3)", "#t");
  check_e("(< -1 1 1 2.0)", "#f");
  check_e("(< 1 2 -1 6.7)", "#f");

  check_e("(>= 2 1)", "#t");
  check_e("(>= 1 2)", "#f");
  check_e("(>= 1 1)", "#t");
  check_e("(>= 3 2.0 1)", "#t");
  check_e("(>= 3 2.0 1 1)", "#t");
  check_e("(>= 3 2.0 1 100)", "#f");

  check_e("(<= 1 2)", "#t");
  check_e("(<= 2 1)", "#f");
  check_e("(<= 1 1)", "#t");
  check_e("(<= 1 2.0 3)", "#t");
  check_e("(<= -1 1 1 2.0)", "#t");
  check_e("(<= 1 2 -1 6.7)", "#f");


  // property tests
  check_e("(zero? 0+0i)", "#t");
  check_e("(zero? -1i)", "#f");
  check_e("(zero? 1.0)", "#f");
  check_e("(zero? 0.0)", "#t");
  check_e("(zero? -1.0)", "#f");
  check_e("(zero? 1)", "#f");
  check_e("(zero? 0)", "#t");
  check_e("(zero? -1)", "#f");

  check_e("(positive? 0+0i)", "#f");
  check_e("(positive? -1i)", "#f");
  check_e("(positive? 1.0)", "#t");
  check_e("(positive? 0.0)", "#f");
  check_e("(positive? -1.0)", "#f");
  check_e("(positive? 1)", "#t");
  check_e("(positive? 0)", "#f");
  check_e("(positive? -1)", "#f");

  check_e("(negative? 0+0i)", "#f");
  check_e("(negative? -1i)", "#f");
  check_e("(negative? 1.0)", "#f");
  check_e("(negative? 0.0)", "#f");
  check_e("(negative? -1.0)", "#t");
  check_e("(negative? 1)", "#f");
  check_e("(negative? 0)", "#f");
  check_e("(negative? -1)", "#t");

  check_e("(even? 0+0i)", "#f");
  check_e("(even? 0.0)", "#f");
  check_e("(even? 2)", "#t");
  check_e("(even? 1)", "#f");
  check_e("(even? 0)", "#t");
  check_e("(even? -1)", "#f");

  check_e("(odd? 1+0i)", "#f");
  check_e("(odd? 1.0)", "#f");
  check_e("(odd? 2)", "#f");
  check_e("(odd? 1)", "#t");
  check_e("(odd? 0)", "#f");
  check_e("(odd? -1)", "#t");


  // min / max
  check_e("(max 1 2 3 4)", "4");
  check_e("(max -1 2.1 3 4)", "4");
  check_e("(integer? (max -1 2.1 3 4))", "#f");

  check_e("(min 1 2 3 4)", "1");
  check_e("(min -1 2.1 3 4)", "-1");
  check_e("(integer? (min -1 2.1 3 4))", "#f");


  // +-*/
  check_e("(+)", "0");
  check_e("(+ 1 )", "1");
  check_e("(+ 1 2)", "3");
  check_e("(+ 1 2 3)", "6");

  check_e("(*)", "1");
  check_e("(* 1 )", "1");
  check_e("(* 1 2)", "2");
  check_e("(* -1 -2 -3)", "-6");

  check_e("(- 1 )", "-1");
  check_e("(- 1 2)", "-1");
  check_e("(- 1 2 3)", "-4");

  check_e("(/ 1)", "1");
  check_e("(/ 1 2)", "0.5");
  check_e("(/ 1 2 2)", "0.25");

  // abs
  check_e("(abs 1)", "1");
  check_e("(abs -7)", "7");

  // integer division
  check_e("(quotient 13 4)", "3");
  check_e("(modulo 13 4)", "1");
  check_e("(remainder 13 4)", "1");
  check_e("(quotient -13 4)", "-3");
  check_e("(modulo -13 4)", "3");
  check_e("(remainder -13 4)", "-1");
  check_e("(quotient 13 -4)", "-3");
  check_e("(modulo 13 -4)", "-3");
  check_e("(remainder 13 -4)", "1");
  check_e("(quotient -13 -4)", "3");
  check_e("(modulo -13 -4)", "-1");
  check_e("(remainder -13 -4)", "-1");

  // gcd / lcm
  check_e("(gcd 32 -36)", "4");
  check_e("(gcd)", "0");
  check_e("(lcm 32 -36)", "288");
  check_e("(lcm)", "1");

  // rational
  // check_e("(numarator (/ 6 4))", "3");
  // check_e("(denominator (/ 6 4))", "2");
  // check_e("(denominator (exact->inexact (/ 6 4)))", "2.0");

  // inexact
  check_e("(floor -4.3)", "-5");
  check_e("(ceiling -4.3)", "-4");
  check_e("(truncate -4.3)", "-4");
  check_e("(round -4.3)", "-4");
  check_e("(floor 3.5)", "3");
  check_e("(ceiling 3.5)", "4");
  check_e("(truncate 3.5)", "3");
  check_e("(round 3.5)", "4");
  // exact
  check_e("(round 7)", "7");

  // rationalize
  // check_e("(rationalize (inexact->exact .3) 1/10)", "1/3");
  // check_e("(rationalize .3 1/10)", "#i1/3");
  
  // real
  // check_e("(exp 2)", "");
  // check_e("(log 2)", "");
  // check_e("(sin 2)", "");
  // check_e("(cos 2)", "");
  // check_e("(tan 2)", "");
  // check_e("(asin 2)", "");
  // check_e("(acos 2)", "");
  // check_e("(atan 2)", "");
  // check_e("(atan 2 3)", "");

  check_e("(sqrt 4)", "2");

  check_e("(expt 2 2)", "4");

  // complex
  // (make-rectangular x1 x2)         ===> z
  //   (make-polar x3 x4)             ===> z
  //   (real-part z)                          ===> x1
  //   (imag-part z)                          ===> x2
  //   (magnitude z)                          ===> |x3|
  //   (angle z)                              ===> xangle

  // exactness conversion
  // (exact->inexact z) 
  // (inexact->exact z) 


  // reader/writer
  // (number->string z) 
  // (number->string z radix) 
// (let ((number number)
//       (radix radix))
//   (eqv? number
//         (string->number (number->string number
//                                         radix)
//                         radix))) -> #t


  check_e("(string->number \"7\")", "7");
  check_e("(string->number \"10\" 8)", "8");
  check_e("(string->number \"100\")", "100");
  check_e("(string->number \"100\" 16)", "256");
  check_e("(string->number \"1e2\")", "100");
  check_e("(string->number \"15##\")", "1500");


  return RESULT;
}
