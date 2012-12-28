#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  check_e("(not #t)", "#f");
  check_e("(not 3)", "#f");
  check_e("(not (list 3))", "#f");
  check_e("(not #f)", "#t");
  check_e("(not '())", "#f");
  check_e("(not (list))", "#f");
  check_e("(not 'nil)", "#f");

  check_e("(boolean? #f)", "#t");
  check_e("(boolean? 0)", "#f");
  check_e("(boolean? '())", "#f");

  return RESULT;
}
