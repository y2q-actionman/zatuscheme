#include "builtin_symbol.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "symbol.hh"

using namespace std;
using namespace Procedure;

namespace {

void sym_to_string(){
  auto arg = pick_args_1();
  auto sym = arg.get<Symbol*>();
  if(!sym){
    fprintf(zs::err, "native func: symbol->string: arg is not symbol! (%s)\n",
            stringify(arg.tag()));
    VM.return_value = {};
    return;
  }

  // TODO: support invariant string!
  VM.return_value = {new String(sym->name())};
}

void sym_from_string(){
  auto arg = pick_args_1();
  auto str = arg.get<String*>();
  if(!str){
    fprintf(zs::err, "native func: string->symbol: arg is not string! (%s)\n",
            stringify(arg.tag()));
    VM.return_value = {};
    return;
  }

  VM.return_value = {intern(VM.symtable, *str)};
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

void install_builtin_symbol(){
}
