#include <cstdio>
#include <cstring>

#include "eval.hh"
#include "stack.hh"
#include "env.hh"
#include "builtin.hh"
#include "symtable.hh"
#include "reader.hh"
#include "test_util.hh"

using namespace std;

static bool result = true;

void check(const char* expr_s, const char* expect_s,
           Env& e, Stack& st, SymTable& symt){
  const auto start_stack_size = st.size();

  auto expr = read_from_string(symt, expr_s);
  if(!expr){
    printf("reader error occured in expr!: %s\n", expr_s);
    result = false;
    return;
  }

  auto expect = read_from_string(symt, expect_s);
  if(!expect){
    printf("reader error occured in expect!: %s\n", expect_s);
    result = false;
    return;
  }

  auto evaled = eval(expr, e, st);
  if(!evaled){
    printf("eval error occured: %s\n", expr_s);
    result = false;
    return;
  }

  if(!eql(evaled, expect)){
    printf("not eql!: %s vs %s\n", expr_s, expect_s);
    result = false;
    return;
  }

  if(start_stack_size != st.size()){
    printf("stack is not cleaned!!: %d -> %d (evaled: %s)\n",
           start_stack_size, st.size(), expr_s);
    result = false;
    return;
  }
}

int main(){
  Env e;
  Stack st;
  SymTable symt;

  install_builtin(e, symt);

  // self-evaluating

  // simple func
  check("(+ 1 1)", "2", e, st, symt);

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
