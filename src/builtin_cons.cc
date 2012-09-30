#include "builtin_cons.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"

using namespace std;
using namespace Procedure;

namespace {

void type_check_pair(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{(arg.tag() == Ptr_tag::cons) && !nullp(arg)};
}

void cons_func(){
  auto args = pick_args<2>();
  VM.return_value = {new Cons(args[0], args[1])};
}


constexpr BuiltinFunc
builtin_func[] = {
  {"pair?", {
      type_check_pair,
      Calling::function, {1, false}}},

  {"cons", {
      cons_func,
      Calling::function, {2, false}}},
};

} // namespace

void install_builtin_cons(){
  for(auto& e : builtin_func){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
