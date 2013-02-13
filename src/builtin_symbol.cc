#include "builtin_symbol.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "symbol.hh"
#include "zs_error.hh"

using namespace std;

Lisp_ptr sym_to_string(){
  ZsArgs args;
  auto sym = args[0].get<Symbol*>();
  if(!sym){
    throw zs_error("native func: symbol->string: arg is not symbol! (%s)\n",
                        stringify(args[0].tag()));
  }

  // TODO: support invariant string!
  return {new String(sym->name())};
}

Lisp_ptr sym_from_string(){
  ZsArgs args;
  auto str = args[0].get<String*>();
  if(!str){
    throw zs_error("native func: string->symbol: arg is not string! (%s)\n",
                        stringify(args[0].tag()));
  }

  return {intern(vm.symtable(), *str)};
}
