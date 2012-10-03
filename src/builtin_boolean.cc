#include "builtin_boolean.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"

using namespace std;
using namespace Procedure;

namespace {

void not_func(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{!arg.get<bool>()};
}

constexpr BuiltinFunc
builtin_func[] = {
  {"boolean?", {
      type_check_pred<Ptr_tag::boolean>, 
      {Calling::function, 1}}},
  {"not", {
      not_func,
      {Calling::function, 1}}}
};

} // namespace

void install_builtin_boolean(){
  for(auto& e : builtin_func){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
