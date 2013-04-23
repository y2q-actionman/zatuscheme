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
    throw zs_error_arg1("symbol->string", "arg is not symbol!", {args[0]});
  }

  // TODO: support invariant string!
  return {zs_new<String>(sym->name())};
}

Lisp_ptr symbol_from_string(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw zs_error_arg1("string->symbol", "arg is not string!", {args[0]});
  }

  return {intern(vm.symtable(), *str)};
}

} // namespace builtin
