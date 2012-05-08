#include <cstring>
#include <string>

#include "decl.hh"
#include "printer.hh"
#include "number.hh"
#include "symtable.hh"
#include "symbol.hh"
#include "cons.hh"
#include "test_util.hh"

using namespace std;

static bool result;

void check(Lisp_ptr input, const char* expect){
  const auto callback = [expect](const char* str){
    fprintf(stdout, "[failed] expected: %s\n\treturned: %s\n",
            expect, str);
  };

  if(!test_on_print(input, expect, callback)){
    result = false;
  }
}

void check_noprint(Lisp_ptr input){
  FILE* f = fopen("/dev/null", "w");
  print(f, input);
  fclose(f);
}


template<typename T>
void check(const T& t, const char* expect){
  check(Lisp_ptr{t}, expect);
}

void check(const char* s, const char* expect){
  string ss{s};
  check(Lisp_ptr{&ss}, expect);
}

void check(Number&& n, const char* expect){
  Number nn{n};
  check(Lisp_ptr{&nn}, expect);
}

template<typename T>
void check_noprint(const T& t){
  check_noprint(Lisp_ptr{t});
}  



int main(){
  result = true;
  SymTable stab;

  // boolean
  check(true, "#t");
  check(false, "#f");

  // char
  check('a', "#\\a");
  check('z', "#\\z");
  check('0', "#\\0");
  check(' ', "#\\ ");
  check('\\', "#\\\\");

  // symbol
  check(stab.intern("hoge"), "hoge");
  check(stab.intern("a b c "), "a b c ");

  // function (should be added in future)
  check_noprint(static_cast<Function*>(nullptr));

  // number
  check(Number(100l), "100");
  check(Number(1.1), "1.1");

  // string
  check("abc", "\"abc\"");
  check("a\"bc", "\"a\"bc\"");

  // port (should be added in future)
  check_noprint(static_cast<Port*>(nullptr));


  // cons, list
  check(Cons::NIL, "()");
  {
    Cons c1{Cons::NIL, Cons::NIL};
    check(&c1, "(())");

    Cons c2{Cons::NIL, Lisp_ptr{&c1}};
    check(&c2, "(() ())");

    Cons c3{Lisp_ptr{true}, Lisp_ptr{&c2}};
    check(&c3, "(#t () ())");

    Cons c4{Lisp_ptr{true}, Lisp_ptr{false}};
    check(&c4, "(#t . #f)");

    Cons c5{Lisp_ptr{&c3}, Lisp_ptr{&c4}};
    check(&c5, "((#t () ()) #t . #f)");
  }

  // vector
  {
    Vector v1;
    check(&v1, "#()");

    v1.push_back(Cons::NIL);
    check(&v1, "#(())");

    v1.push_back(Cons::NIL);
    check(&v1, "#(() ())");

    Vector v2;
    v2.push_back(Lisp_ptr{&v1});
    check(&v2, "#(#(() ()))");
  }

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}