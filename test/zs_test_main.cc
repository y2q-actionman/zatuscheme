#include "zs.hh"
#include <fstream>

using namespace std;

int main(int argc, const char* argz[]){
  if(argc <= 1){
    return 0;
  }    
  
  zs_init();

  ifstream ifs{argz[1]};
  load_internal(ifs);
  eval();
  return zs_exit_status();
}
