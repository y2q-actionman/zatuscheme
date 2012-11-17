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

void sym_to_string(){
  auto arg = pick_args_1();
  auto sym = arg.get<Symbol*>();
  if(!sym){
    throw make_zs_error("native func: symbol->string: arg is not symbol! (%s)\n",
                        stringify(arg.tag()));
  }

  // TODO: support invariant string!
  vm.return_value[0] = {new String(sym->name())};
}

void sym_from_string(){
  auto arg = pick_args_1();
  auto str = arg.get<String*>();
  if(!str){
    throw make_zs_error("native func: string->symbol: arg is not string! (%s)\n",
                        stringify(arg.tag()));
  }

  vm.return_value[0] = {intern(vm.symtable(), *str)};
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
