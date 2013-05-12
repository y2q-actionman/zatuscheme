#include "builtin_srfi.hh"
#include "zs_error.hh"

using namespace std;

namespace builtin {

Lisp_ptr error(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  throw zs_error_arg1(nullptr, *str);
}

} // namespace builtin
