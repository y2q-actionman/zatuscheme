#include "zs.hh"

#define REPL_PROMPT ">> "

int main(){
  install_builtin();

  while(1){
    printf(REPL_PROMPT);
    fflush(stdout);
    VM.code.push(read(zs::in));
    eval();
    print(zs::out, VM.return_value);
    putchar('\n');
  }

  return 0;
}
