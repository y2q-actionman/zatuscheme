#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "procedure.hh"

// for internal direct 'goto'.
void proc_enter_entrypoint(Lisp_ptr);

// primitives for syntax call
constexpr VMop vm_op_nop = nullptr;
void vm_op_proc_enter();
void vm_op_move_values();
void vm_op_leave_frame();
void vm_op_if();
void vm_op_set();
void vm_op_local_set();
void vm_op_begin();
void vm_op_force();
void vm_op_leave_winding();
void vm_op_save_values_and_enter();
void vm_op_get_current_env();

// main loop
void eval();

bool is_self_evaluating(Lisp_ptr);

// builtin funcs near evaluator
Lisp_ptr let_internal(Procedure::Entering);

// for debug
const char* stringify(VMop);

#endif // EVAL_HH
