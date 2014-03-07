#include "zs.hh"
#include <sstream>

using namespace std;

int main(int argc, const char* argz[]){
  static const char* default_code[] = {"", "(read-eval-print-loop)"};

  zs_init();

  if(argc <= 1){
    argc = sizeof(default_code) / sizeof(default_code[0]);
    argz = default_code;
  }    

  for(int i = 1; i < argc; ++i){
    istringstream iss{argz[i]};
    load_from_stream(iss);
    eval();
  }

  return zs_exit_status();
}
