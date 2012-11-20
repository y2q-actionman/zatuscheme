#include <exception>
#include <cstdio>

#include "zs.hh"

using namespace std;

static void term_handle(){
  std::set_terminate(nullptr);
  fprintf(stderr, "uncaught exception! aborting..\n\n");

  print(stderr, vm);
  fputc('\n', stderr);

  try{
    rethrow_exception(current_exception());
  }catch(std::exception& e){
    fprintf(stderr, "what: %s\n", e.what());
  }
}

void zs_init(){
  std::set_terminate(term_handle);
  install_builtin();
}
