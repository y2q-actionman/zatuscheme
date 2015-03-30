#include <cstdlib>
#include <exception>
#include <ios>

#include "zs.hh"

using namespace std;

namespace zs {

void zs_init(){
  std::ios::sync_with_stdio(false);
  std::set_terminate(zs_terminate_handler);
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

} // namespace zs
