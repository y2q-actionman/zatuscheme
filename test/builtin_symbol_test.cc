#include "zs.hh"
#include "test_util.hh"
#include "config.h"

#ifndef CASE_INSENSITIVE
# if defined(USE_CASE_UPPER) || defined(USE_CASE_LOWER)
#  define CASE_INSENSITIVE
# endif
#endif

int main(){
  zs_init();

  check_e("(symbol? 'foo)", "#t");
  check_e("(symbol? (car '(a b)))", "#t");
  check_e("(symbol? \"bar\")", "#f");
  check_e("(symbol? 'nil)", "#t");
  check_e("(symbol? '())", "#f");
  check_e("(symbol? #f)", "#f");


  check_e("(symbol->string 'flying-fish)", "\"flying-fish\"");

#ifdef CASE_INSENSITIVE
  check_e("(symbol->string 'Martin)", "\"martin\"");
#else
  check_e("(symbol->string 'Martin)", "\"Martin\"");
#endif

  check_e("(symbol->string (string->symbol \"Malvina\"))", "\"Malvina\"");


#ifdef CASE_INSENSITIVE
  check_e("(eq? 'mISSISSIppi 'mississippi)", "#t");
#else
  check_e("(eq? 'mISSISSIppi 'mISSISSIppi)", "#t");
#endif

  check_e("(string->symbol \"mISSISSIppi\")", "mISSISSIppi");

#ifdef CASE_INSENSITIVE
  check_e("(eq? 'bitBlt (string->symbol \"bitBlt\"))", "#f");
#else
  check_e("(eq? 'bitBlt (string->symbol \"bitBlt\"))", "#t");
#endif

  check_e("(eq? 'JollyWog (string->symbol (symbol->string 'JollyWog)))", "#t");

  //check_e("(string=? \"K. Harper, M.D.\" (symbol->string (string->symbol \"K. Harper, M.D.\")))", "#t");
  
  return RESULT;
}
