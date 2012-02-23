#include <cstring>
#include <string>

#include "printer.hh"
#include "number.hh"
#include "symtable.hh"
#include "symbol.hh"
#include "cons.hh"

using namespace std;

static bool result;

void check(Lisp_ptr input, const char* expect){
  char* buf;
  size_t buf_size;

  FILE* tmp_f = open_memstream(&buf, &buf_size);
  print(tmp_f, input);
  fclose(tmp_f);

  if(strncmp(expect, buf, strlen(expect)) != 0){
    fprintf(stdout, "[failed] expected: %s\n\treturned: %s\n"
            // "bit repl: %x\n"
            , expect, buf
            // , input.base_
            );
    result = false;
  }

  free(buf);
}

void check_noprint(Lisp_ptr input){
  FILE* f = tmpfile();
  print(f, input);
  fclose(f);
}


void check(bool b, const char* expect){
  check(Lisp_ptr{b}, expect);
}

void check(char c, const char* expect){
  check(Lisp_ptr{c}, expect);
}

void check(Cons* c, const char* expect){
  check(Lisp_ptr{c}, expect);
}

void check(Symbol* sym, const char* expect){
  check(Lisp_ptr{sym}, expect);
}

void check(const char* s, const char* expect){
  string ss{s};
  Long_ptr lp{&ss};
  check(Lisp_ptr{&lp}, expect);
}

void check(Number&& n, const char* expect){
  Number nn{n};
  Long_ptr lp{&nn};
  check(Lisp_ptr{&lp}, expect);
}

void check_noprint(Function* f){
  Long_ptr lp{f};
  check_noprint(Lisp_ptr{&lp});
}  

void check_noprint(Port* f){
  Long_ptr lp{f};
  check_noprint(Lisp_ptr{&lp});
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


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
