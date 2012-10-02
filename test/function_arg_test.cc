#include <cstdlib>
#include <cassert>

#include "zs.hh"
#include "test_util.hh"
#include "describe.hh"

using namespace Procedure;

static bool result = true;

bool operator!=(const ProcInfo& a, const ProcInfo& b){
  // ignores ProcInfo::head..
  return (a.required_args != b.required_args)
    || (a.variadic != b.variadic);
}

void check(const char* input, const ProcInfo& expect){
  auto p = read_from_string(input);
  assert(p);

  auto argi = parse_func_arg(p);
  if(!argi || argi != expect){
    fprintf(zs::err, "[failed] unexpected failure: input='%s', expected=",
            input);
    describe(zs::err, expect);
    fprintf(zs::err, " got=");
    describe(zs::err, argi);
    fputc('\n', zs::err);

    result = false;
  }
}

void check_uninit(const char* input){
  auto p = read_from_string(input);
  assert(p);

  auto argi = parse_func_arg(p);
  if(argi){
    fprintf(zs::err, "[failed] unexpected succeed: input='%s', arginfo=",
           input);
    describe(zs::err, argi);
    fputc('\n', zs::err);

    result = false;
  }
}

#include "cons.hh"

int main(){
  // arginfo test
  {
    with_null_stream wns;
    check_uninit("#t");
    check_uninit("#\\h");
    check_uninit("100");
    check_uninit("1.01");
    check_uninit("#()");
  }

  check("()", {0});
  check("(a)", {1});
  check("(a b)", {2});
  check("(a b c)", {3});
  check("a", {0, Variadic::t});
  check("(a . b)", {1, Variadic::t});
  check("(a b . c)", {2, Variadic::t});
  check("(a b c . d)", {3, Variadic::t});

  {
    with_null_stream wns;
    check_uninit("(a 1 b)");
    check_uninit("(a 1 . b)");
    check_uninit("(a b . 1)");
  }
  
  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
