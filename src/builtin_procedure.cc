#include "builtin_procedure.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"

using namespace std;
using namespace Procedure;

namespace {

void type_check_procedure(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{(arg.tag() == Ptr_tag::i_procedure)
                             || (arg.tag() == Ptr_tag::n_procedure)};
}

} // namespace

const BuiltinFunc
builtin_procedure[] = {
  {"procedure?", {
      type_check_procedure,
      {Calling::function, 1}}},
};

const size_t builtin_procedure_size = sizeof(builtin_procedure) / sizeof(builtin_procedure[0]);
