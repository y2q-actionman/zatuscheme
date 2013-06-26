#include "builtin_symbol.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "symbol.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace builtin {

Lisp_ptr symbol_to_string(ZsArgs args){
  auto sym = args[0].get<Symbol*>();
  if(!sym){
    throw builtin_type_check_failed(Ptr_tag::symbol, {args[0]});
  }

  // TODO: support invariant string!
  return {zs_new<String>(sym->name())};
}

Lisp_ptr symbol_from_string(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(Ptr_tag::symbol, {args[0]});
  }

  return {intern(*vm.symtable, *str)};
}

} // namespace builtin
