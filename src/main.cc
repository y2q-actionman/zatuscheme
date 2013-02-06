#include "zs.hh"
#include <iostream>

using namespace std;

int main(){
  zs_init();

#ifndef HARD_REPL
  vm.code.push_back(make_cons_list({intern(vm.symtable(), "read-eval-print-loop")}));
  eval();
#else
  while(1){
    cout << ">> " << flush;
    auto val = read(cin);
    vm.code.push_back(val);
    eval();
    print(cout, vm.return_value[0]);
    cout << endl;
  }
#endif
  
  return 0;
}
