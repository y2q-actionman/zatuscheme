#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "procedure.hh"

// primitives for syntax call
constexpr VMop vm_op_nop = nullptr;
void vm_op_call();
void vm_op_proc_enter();
void vm_op_move_values();
void vm_op_leave_frame();
void vm_op_if();
void vm_op_set();
void vm_op_local_set();
void vm_op_force();
void vm_op_leave_winding();
void vm_op_save_values_and_enter();
void vm_op_get_current_env();
void vm_op_raise();
void vm_op_unwind_guard();

// main loop
void eval();

// for debug
const char* stringify(VMop);
extern bool dump_mode;
extern unsigned instruction_counter;
extern const unsigned gc_invoke_interval;

#endif // EVAL_HH
