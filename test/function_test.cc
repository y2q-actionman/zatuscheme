#include <cstdlib>
#include <cassert>

#include "function.hh"
#include "reader.hh"
#include "builtin.hh"
#include "symtable.hh"
#include "test_util.hh"

static bool result = true;

namespace the {
  static SymTable symtable;
}

bool operator!=(const Function::ArgInfo& a, const Function::ArgInfo& b){
  // ignores ArgInfo::head..
  return (a.required_args != b.required_args)
    || (a.variadic != b.variadic);
}

void check(const char* input, const Function::ArgInfo& expect){
  auto p = read_from_string(the::symtable, input);
  assert(p);

  auto argi = parse_func_arg(p);
  if(!argi || argi != expect){
    printf("[failed] unexpected failure: input='%s', expected=",
           input);
    describe(stdout, expect);
    printf(" got=");
    describe(stdout, argi);
    putchar('\n');

    result = false;
  }
}

void check_uninit(const char* input){
  auto p = read_from_string(the::symtable, input);
  assert(p);

  auto argi = parse_func_arg(p);
  if(argi){
    printf("[failed] unexpected succeed: input='%s', arginfo=",
           input);
    describe(stdout, argi);
    putchar('\n');

    result = false;
  }
}

#include "cons.hh"

int main(){
  // arginfo test
  check_uninit("#t");
  check_uninit("#\\h");
  check_uninit("100");
  check_uninit("1.01");
  check_uninit("#()");

  check("()", {{}, 0, false});
  check("(a)", {{}, 1, false});
  check("(a b)", {{}, 2, false});
  check("(a b c)", {{}, 3, false});
  check("a", {{}, 0, true});
  check("(a . b)", {{}, 1, true});
  check("(a b . c)", {{}, 2, true});
  check("(a b c . d)", {{}, 3, true});

  check_uninit("(a 1 b)");
  check_uninit("(a 1 . b)");
  check_uninit("(a b . 1)");

  
  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
