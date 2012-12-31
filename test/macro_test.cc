#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // traditional macro (extension)
  check_e_success("(define tmp-func (lambda (x) `(define ,x ',x)))");
  check_e("(tmp-func 'a)", "(define a (quote a))");

  check_e_success("(define-syntax tmp-macro (traditional-transformer tmp-func))");
  check_e_undef("a");
  check_e_success("(tmp-macro a)");
  check_e("a", "a");

  return RESULT;
}
