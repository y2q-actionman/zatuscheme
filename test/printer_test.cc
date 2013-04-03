#include "zs.hh"
#include "test_util.hh"

using namespace std;

int main(){
  // boolean
  check_p(Lisp_ptr{true}, "#t");
  check_p(Lisp_ptr{false}, "#f");

  // char
  check_p(Lisp_ptr{'a'}, "#\\a");
  check_p(Lisp_ptr{'z'}, "#\\z");
  check_p(Lisp_ptr{'0'}, "#\\0");
  check_p(Lisp_ptr{' '}, "#\\space");
  check_p(Lisp_ptr{'\\'}, "#\\\\");

  // symbol
  check_p(intern(vm.symtable(), "hoge"), "hoge");
  check_p(intern(vm.symtable(), "a b c "), "a b c ");

  // function (should be added in future)
  check_p_success(static_cast<IProcedure*>(nullptr));
  check_p_success(static_cast<const NProcedure*>(nullptr));

  // number
  check_p({Ptr_tag::integer, 100}, "100");
  check_p(new double(1.1), "1.1");

  // string
  check_p(new String("abc"), "\"abc\"");
  check_p(new String("a\"bc"), "\"a\\\"bc\"");

  // port (should be added in future)
  check_p_success(static_cast<InputPort*>(nullptr));
  check_p_success(static_cast<OutputPort*>(nullptr));


  // cons, list
  check_p(Cons::NIL, "()");
  {
    Cons c1{Cons::NIL, Cons::NIL};
    check_p(&c1, "(())");

    Cons c2{Cons::NIL, Lisp_ptr{&c1}};
    check_p(&c2, "(() ())");

    Cons c3{Lisp_ptr{true}, Lisp_ptr{&c2}};
    check_p(&c3, "(#t () ())");

    Cons c4{Lisp_ptr{true}, Lisp_ptr{false}};
    check_p(&c4, "(#t . #f)");

    Cons c5{Lisp_ptr{&c3}, Lisp_ptr{&c4}};
    check_p(&c5, "((#t () ()) #t . #f)");
  }

  // vector
  {
    Vector v1;
    check_p(&v1, "#()");

    v1.push_back(Cons::NIL);
    check_p(&v1, "#(())");

    v1.push_back(Cons::NIL);
    check_p(&v1, "#(() ())");

    Vector v2;
    v2.push_back(Lisp_ptr{&v1});
    check_p(&v2, "#(#(() ()))");
  }

  return RESULT;
}
