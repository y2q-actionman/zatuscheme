#include "zs.hh"

#define REPL_PROMPT ">> "

int main(){
  install_builtin();

  while(1){
    printf(REPL_PROMPT);
    fflush(stdout);
    vm.code.push(read(zs::in));
    eval();
    print(zs::out, vm.return_value[0]);
    putchar('\n');
  }

  return 0;
}
