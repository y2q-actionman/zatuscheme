#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"

constexpr VMop vm_op_nop = nullptr;
constexpr VMop vm_op_arg_bottom = nullptr;

void vm_op_if();
void vm_op_set();
void vm_op_local_set();
void vm_op_quasiquote();

void let_internal(bool sequencial, bool early_bind);

// main loop
void eval();

#endif // EVAL_HH
