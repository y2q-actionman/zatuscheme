#include <exception>
#include <iostream>

#include "zs.hh"

using namespace std;

static void term_handle(){
  std::set_terminate(nullptr);

  cerr << "uncaught exception! aborting..\n\n";
  cerr << vm << endl;

  try{
    rethrow_exception(current_exception());
  }catch(std::exception& e){
    cerr << "what: " << e.what() << endl;
  }
}

void zs_init(){
  std::set_terminate(term_handle);
  install_builtin();
}
