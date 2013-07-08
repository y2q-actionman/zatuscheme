#include "builtin_symbol.hh"
#include "lisp_ptr.hh"
#include "symbol.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace builtin {

Lisp_ptr symbol_to_string(ZsArgs args){
  check_type(Ptr_tag::symbol, args[0]);

  auto sym = args[0].get<Symbol*>();
  // TODO: support invariant string!
  return {zs_new<String>(sym->name())};
}

Lisp_ptr symbol_from_string(ZsArgs args){
  check_type(Ptr_tag::string, args[0]);

  auto str = args[0].get<String*>();
  return {intern(*vm.symtable, *str)};
}

} // namespace builtin
