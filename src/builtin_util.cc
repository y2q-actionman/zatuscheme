#include "builtin_util.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include <iterator>

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

Lisp_ptr pick_args_1(){
  Lisp_ptr ret[1];
  pick_args(std::begin(ret), std::end(ret));
  return ret[0];
}

