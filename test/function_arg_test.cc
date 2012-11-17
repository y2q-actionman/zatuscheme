#include <cstdlib>
#include <cassert>

#include "zs.hh"
#include "test_util.hh"

using namespace Procedure;

static bool result = true;

typedef decltype(parse_func_arg({})) ArgT;

bool operator!(const ArgT& a){
  return (a.first < 0);
}

bool operator!=(const ArgT& a, const ArgT& b){
  return (a.first != b.first) || (a.second != b.second);
}

void check(const char* input, const ArgT& expect){
  auto p = read_from_string(input);
  assert(p);

  auto argi = parse_func_arg(p);
  if(!argi || argi != expect){
    fprintf(zs::err, "[failed] unexpected failure: input='%s', expected=[%d, %d], got=[%d, %d]\n",
            input, argi.first, argi.second, expect.first, expect.second);

    result = false;
  }
}

void check_uninit(const char* input){
  auto p = read_from_string(input);
  assert(p);

  auto argi = parse_func_arg(p);
  if(!!argi){
    fprintf(zs::err, "[failed] unexpected succeed: input='%s', arginfo=[%d, %d]\n",
           input, argi.first, argi.second);

    result = false;
  }
}

#include "cons.hh"

int main(){
  // arginfo test
  with_expect_error([]() -> void {
      check_uninit("#t");
      check_uninit("#\\h");
      check_uninit("100");
      check_uninit("1.01");
      check_uninit("#()");
    });

  check("()", {0, Variadic::f});
  check("(a)", {1, Variadic::f});
  check("(a b)", {2, Variadic::f});
  check("(a b c)", {3, Variadic::f});
  check("a", {0, Variadic::t});
  check("(a . b)", {1, Variadic::t});
  check("(a b . c)", {2, Variadic::t});
  check("(a b c . d)", {3, Variadic::t});

  with_expect_error([]() -> void {
      check_uninit("(a 1 b)");
      check_uninit("(a 1 . b)");
      check_uninit("(a b . 1)");
    });
  
  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
