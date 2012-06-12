#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "vm.hh"

enum class VM_op{
  nop = 0,
    if_,
    set_,
    funcall,
    arg_push,
    arg_bottom,
    interpreted_call,
    native_call,
    leave_frame
};

void eval();

#endif // EVAL_HH
