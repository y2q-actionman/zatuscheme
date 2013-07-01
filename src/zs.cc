#include <cstdlib>
#include <iostream>
#include <exception>

#include "zs.hh"

using namespace std;

static void term_handle() noexcept{
  std::set_terminate(nullptr);

  try{
    rethrow_exception(current_exception());
  }catch(const Lisp_ptr& errobj){
    cerr << "uncaught exception!\n"
         << "raised object: " << errobj << '\n'
         << "vm dump: \n" << vm << '\n'
         << endl;
  }catch(const std::exception& e){
    cerr << "uncaught system exception!\n"
         << "what: " << e.what()
         << endl;
  }catch(...){
    cerr << "unexpected internal exception!\n"
         << endl;
  }

  cerr << "Aborting..." << endl;
  abort();
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
