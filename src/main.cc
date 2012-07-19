#include <cstdio>
#include "zs.hh"

#define REPL_PROMPT ">> "

int main(){
  install_builtin();

  while(1){
    printf(REPL_PROMPT);
    VM.code.push(read(stdin));
    eval();
    print(stdout, VM.return_value);
    putchar('\n');
  }

  return 0;
}
