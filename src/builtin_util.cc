#include "builtin_util.hh"
#include "lisp_ptr.hh"
#include "vm.hh"

void procedure_list(){
  VM.return_value() = stack_to_list(VM.stack(), false);
}

void procedure_list_star(){
  VM.return_value() = stack_to_list(VM.stack(), true);
}

void procedure_vector(){
  auto v = new Vector;
  stack_to_vector(VM.stack(), *v);
  VM.return_value() = Lisp_ptr{v};
}

