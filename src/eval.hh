#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "vm.hh"

void whole_function_error();
void whole_function_unimplemented();
void whole_function_pass_through();
void whole_function_quote();
void whole_function_if();
void whole_function_lambda();
void whole_function_set();
void whole_function_begin();

void eval();

#endif // EVAL_HH
