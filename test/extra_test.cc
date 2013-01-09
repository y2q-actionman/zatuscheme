#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  // eq hashcode
  check_e_success("(= (eq-hash #t) (eq-hash #t))");
  check_e_success("(= (eq-hash #f) (eq-hash #f))");

  check_e_success("(= (eq-hash 'a) (eq-hash 'a))");
  check_e_success("(not (= (eq-hash 'a) (eq-hash 'b)))");

  return RESULT;
}
