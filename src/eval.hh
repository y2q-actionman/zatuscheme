#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "procedure.hh"

// primitives for syntax call
constexpr VMop vm_op_nop = nullptr;
// constexpr VMop vm_op_arg_bottom = nullptr;

void vm_op_leave_frame();
void vm_op_if();
void vm_op_set();
void vm_op_local_set();
void vm_op_begin();

Lisp_ptr let_internal(Procedure::Entering);


// for internal direct 'goto'.
void proc_enter_entrypoint(Lisp_ptr);

// main loop
void eval();

// builtin funcs near evaluator
Lisp_ptr apply_func();
Lisp_ptr func_force();
Lisp_ptr call_with_values();
Lisp_ptr call_cc();
Lisp_ptr dynamic_wind();
Lisp_ptr capture_env();

// for debug
const char* stringify(VMop);

#endif // EVAL_HH
