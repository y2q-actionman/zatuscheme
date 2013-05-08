#include "zs.hh"

using namespace std;

int main(int argc, const char* argz[]){
  zs_init();

  if(argc <= 1){
    return 1;
  }    

  load_internal(argz[1]);
  return 0;
}
