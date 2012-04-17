#include <cstdio>
#include <cstring>

#include "eval.hh"
#include "stack.hh"
#include "env.hh"
#include "builtin.hh"
#include "symtable.hh"
#include "reader.hh"

using namespace std;

static bool result = true;

void check(const char* expr_s, const char* expect_s,
           Env& e, Stack& st, SymTable& symt){
  FILE* expr_f = fmemopen((void*)expr_s, strlen(expr_s), "r");

  Lisp_ptr expr = read(symt, expr_f);

  Lisp_ptr ret = eval(expr, e, st);
  
  fclose(expr_f);
}

int main(){
  Env e;
  Stack st;
  SymTable symt;

  install_builtin(e, symt);

  check("(+ 1 1)", "2", e, st, symt);

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
