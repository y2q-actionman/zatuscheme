#include "builtin_boolean.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"

using namespace std;
using namespace Procedure;

namespace {

void not_func(){
  ArgAccessor arg{1};
  vm.return_value[0] = Lisp_ptr{!arg[0].get<bool>()};
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
