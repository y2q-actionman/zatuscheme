#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "procedure.hh"

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
void load(Port*);
void apply_func();

#endif // EVAL_HH
