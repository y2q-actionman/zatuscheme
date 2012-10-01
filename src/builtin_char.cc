#include "builtin_char.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"

using namespace std;
using namespace Procedure;

namespace {

constexpr BuiltinFunc
builtin_func[] = {
  {"char?", {
      type_check_pred<Ptr_tag::character>,
      Calling::function, {1, false}}},
};

} // namespace

void install_builtin_char(){
  for(auto& e : builtin_func){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
