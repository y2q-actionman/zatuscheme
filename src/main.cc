#include "zs.hh"

#define REPL_PROMPT ">> "

int main(){
  install_builtin();

  vm.code.push_back(make_cons_list({intern(vm.symtable(), "read-eval-print-loop")}));
  eval();

  return 0;
}
