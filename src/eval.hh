#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "vm.hh"

enum class VM_op{
  nop = 0,
    if_,
    set_,
    call,
    arg_push,
    arg_push_list,
    arg_bottom,
    interpreted_call,
    native_call,
    leave_frame,
    macro_call,
    quasiquote,
    quasiquote_list,
    quasiquote_vector
};

void eval();

#endif // EVAL_HH
