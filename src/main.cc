#include "zs.hh"

int main(){
  zs_init();

  vm.code.push_back(make_cons_list({intern(vm.symtable(), "read-eval-print-loop")}));
  eval();

  return 0;
}
