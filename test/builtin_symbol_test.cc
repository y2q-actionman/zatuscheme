#include "zs.hh"
#include "test_util.hh"

static bool result = true;

void check(const char* input, const char* expect){
  result = read_eval_print_test(input, expect);
}

// TODO: if case-insensitivity is supported, define CASE_INSENSITIVE


int main(){
  install_builtin();
  install_builtin_syntax();
  install_builtin_cons();
  install_builtin_symbol();

  check("(symbol? 'foo)", "#t");
  check("(symbol? (car '(a b)))", "#t");
  check("(symbol? \"bar\")", "#f");
  check("(symbol? 'nil)", "#t");
  check("(symbol? '())", "#f");
  check("(symbol? #f)", "#f");


  check("(symbol->string 'flying-fish)", "\"flying-fish\"");

#if CASE_INSENSITIVE
  check("(symbol->string 'Martin)", "\"martin\"");
#else
  check("(symbol->string 'Martin)", "\"Martin\"");
#endif

  check("(symbol->string (string->symbol \"Malvina\"))", "\"Malvina\"");


#if CASE_INSENSITIVE
  check("(eq? 'mISSISSIppi 'mississippi)", "#t");
#else
  check("(eq? 'mISSISSIppi 'mISSISSIppi)", "#t");
#endif

  check("(string->symbol \"mISSISSIppi\")", "mISSISSIppi");

#if CASE_INSENSITIVE
  check("(eq? 'bitBlt (string->symbol \"bitBlt\"))", "#f");
#else
  check("(eq? 'bitBlt (string->symbol \"bitBlt\"))", "#t");
#endif

  check("(eq? 'JollyWog (string->symbol (symbol->string 'JollyWog)))", "#t");

  //check("(string=? \"K. Harper, M.D.\" (symbol->string (string->symbol \"K. Harper, M.D.\")))", "#t");
  

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

