#include "builtin_extra.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"

using namespace std;
using namespace Procedure;

namespace {

void to_macro_procedure(){
  auto arg1 = pick_args_1();

  if(arg1.tag() != Ptr_tag::i_procedure){
    fprintf(zs::err, "to-macro-procedure: error: should be called with interpreted proc\n");
    VM.return_value = {};
    return;
  }

  auto proc = arg1.get<IProcedure*>();
  auto info = *proc->info();
  info.calling = Calling::macro;

  VM.return_value = new IProcedure(proc->get(), 
                                   info, proc->arg_head(),
                                   proc->closure());
}

} // namespace

const BuiltinFunc
builtin_extra[] = {
  {"to-macro-procedure", {
      to_macro_procedure,
      {Calling::function, 1}}}
};

const size_t builtin_extra_size = sizeof(builtin_extra) / sizeof(builtin_extra[0]);
