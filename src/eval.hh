#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "procedure.hh"

// primitives for syntax call
constexpr VMop vm_op_nop = nullptr;
constexpr VMop vm_op_arg_bottom = nullptr;

void vm_op_leave_frame();
void vm_op_if();
void vm_op_set();
void vm_op_local_set();
void vm_op_quasiquote();

void let_internal(Procedure::Sequencial, Procedure::EarlyBind);

// main loop
void eval();

// builtin funcs near evaluator
void apply_func();
void func_force();
void call_with_values();
void call_cc();

#endif // EVAL_HH
