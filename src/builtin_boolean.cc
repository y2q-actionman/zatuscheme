#include "builtin_boolean.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

Lisp_ptr not_func(){
  ZsArgs args{1};
  return Lisp_ptr{!args[0].get<bool>()};
}

} // namespace

const BuiltinFunc
builtin_boolean[] = {
  {"boolean?", {
      type_check_pred<Ptr_tag::boolean>, 
      {Calling::function, 1}}},
  {"not", {
      not_func,
      {Calling::function, 1}}}
};

const size_t builtin_boolean_size = sizeof(builtin_boolean) / sizeof(builtin_boolean[0]);
