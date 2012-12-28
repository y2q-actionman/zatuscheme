#include "zs.hh"
#include "test_util.hh"

// TODO: if case-insensitivity is supported, define CASE_INSENSITIVE


int main(){
  zs_init();

  check_e("(symbol? 'foo)", "#t");
  check_e("(symbol? (car '(a b)))", "#t");
  check_e("(symbol? \"bar\")", "#f");
  check_e("(symbol? 'nil)", "#t");
  check_e("(symbol? '())", "#f");
  check_e("(symbol? #f)", "#f");


  check_e("(symbol->string 'flying-fish)", "\"flying-fish\"");

#if CASE_INSENSITIVE
  check_e("(symbol->string 'Martin)", "\"martin\"");
#else
  check_e("(symbol->string 'Martin)", "\"Martin\"");
#endif

  check_e("(symbol->string (string->symbol \"Malvina\"))", "\"Malvina\"");


#if CASE_INSENSITIVE
  check_e("(eq? 'mISSISSIppi 'mississippi)", "#t");
#else
  check_e("(eq? 'mISSISSIppi 'mISSISSIppi)", "#t");
#endif

  check_e("(string->symbol \"mISSISSIppi\")", "mISSISSIppi");

#if CASE_INSENSITIVE
  check_e("(eq? 'bitBlt (string->symbol \"bitBlt\"))", "#f");
#else
  check_e("(eq? 'bitBlt (string->symbol \"bitBlt\"))", "#t");
#endif

  check_e("(eq? 'JollyWog (string->symbol (symbol->string 'JollyWog)))", "#t");

  //check_e("(string=? \"K. Harper, M.D.\" (symbol->string (string->symbol \"K. Harper, M.D.\")))", "#t");
  
  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
