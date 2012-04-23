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
  const auto start_stack_size = st.size();
  Lisp_ptr expr, expect, evaled;
  FILE* expr_f = fmemopen((void*)expr_s, strlen(expr_s), "r");
  FILE* expect_f = fmemopen((void*)expect_s, strlen(expect_s), "r");

  expr = read(symt, expr_f);
  if(!expr){
    printf("reader error occured in expr!: %s\n", expr_s);
    result = false;
    goto end;
  }

  expect = read(symt, expect_f);
  if(!expect){
    printf("reader error occured in expect!: %s\n", expect_s);
    result = false;
    goto end;
  }

  evaled = eval(expr, e, st);
  if(!evaled){
    printf("eval error occured: %s\n", expr_s);
    result = false;
    goto end;
  }

  if(!eql(evaled, expect)){
    printf("not eql!: %s vs %s\n", expr_s, expect_s);
    result = false;
    goto end;
  }

  if(start_stack_size != st.size()){
    printf("stack is not cleaned!!: %d -> %d (evaled: %s)\n",
           start_stack_size, st.size(), expr_s);
    result = false;
    goto end;
  }
  
 end:
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
