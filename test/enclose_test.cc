#include <cstdlib>
#include <cassert>

#include "eval.hh"
#include "test_util.hh"
#include "builtin.hh"

static bool result = true;

typedef VM_t::Env Env;

int main(){
  // testing null env, not closed objs.
  {
    Env e;

    auto exp = read_from_string("(+ 1 2 #t #\\Space #(1 2 3) \"aaa\" () )");
    enclose(exp, e);

    if(e.size() != 0){
      printf("[failed] closed %d elems from NULL env!\n",
             e.size());
      result = false;
    }
  }

  // testing on '+' proc
  {
    install_builtin();

    Env e;

    auto exp = read_from_string("(+ 1 2)");
    enclose(exp, e);

    auto plus_exp = read_from_string("+");
    assert(plus_exp.get<Symbol*>());

    if(e.find(plus_exp.get<Symbol*>()) == e.end()){
      printf("[failed] closure cannot capture '+' operator!\n");
      result = false;
    }
  }

  // testing defined variables

  // testing various syntaxes

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

