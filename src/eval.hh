#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "vm.hh"

void vm_op_error();
void vm_op_pass_through();
void vm_op_quote();
void vm_op_begin();

void eval();

#endif // EVAL_HH
