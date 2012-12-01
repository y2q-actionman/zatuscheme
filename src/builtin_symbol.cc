#include "builtin_symbol.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "symbol.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

Lisp_ptr sym_to_string(){
  ZsArgs args{1};
  auto sym = args[0].get<Symbol*>();
  if(!sym){
    throw make_zs_error("native func: symbol->string: arg is not symbol! (%s)\n",
                        stringify(args[0].tag()));
  }

  // TODO: support invariant string!
  return {new String(sym->name())};
}

Lisp_ptr sym_from_string(){
  ZsArgs args{1};
  auto str = args[0].get<String*>();
  if(!str){
    throw make_zs_error("native func: string->symbol: arg is not string! (%s)\n",
                        stringify(args[0].tag()));
  }

  return {intern(vm.symtable(), *str)};
}

} // namespace

const BuiltinFunc
builtin_symbol[] = {
  {"symbol?", {
      type_check_pred<Ptr_tag::symbol>,
      {Calling::function, 1}}},
  {"symbol->string", {
      sym_to_string,
      {Calling::function, 1}}},
  {"string->symbol", {
      sym_from_string,
      {Calling::function, 1}}}
};

const size_t builtin_symbol_size = sizeof(builtin_symbol) / sizeof(builtin_symbol[0]);
