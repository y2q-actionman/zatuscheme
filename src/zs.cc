#include <exception>
#include <iostream>
#include <cstdlib>

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
  std::ios::sync_with_stdio(false);
  std::set_terminate(term_handle);
  install_builtin();
}

int zs_exit_status(){
  auto val = vm.return_value_1();
  if(val.tag() == Ptr_tag::integer){
    return val.get<int>();
  }else if(val.tag() == Ptr_tag::boolean){
    return (val.get<bool>()) ? EXIT_SUCCESS : EXIT_FAILURE;
  }else{
    return (val) ? EXIT_SUCCESS : EXIT_FAILURE;
  }
}
