#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "vm.hh"

constexpr VMop vm_op_arg_bottom = nullptr;

/*
  stack = (args, arg_bottom)
  ----
  ret = undef
*/
void whole_function_error();

/*
  stack = (args, arg_bottom)
  ----
  ret = undef
*/
void whole_function_unimplemented();

/*
  stack = (args, arg_bottom)
  ----
  ret = args
*/
void whole_function_pass_through();

/*
  stack = (args, arg_bottom)
  ----
  ret = arg[0]
*/
void whole_function_quote();

/*
  stack = (args, arg_bottom)
  ----
  ret = proc.
*/
void whole_function_lambda();

/*
  stack = (args, arg_bottom)
  ----
  code = (test, VM::if)
  stack = (consequent, alternative)
*/
void whole_function_if();

/*
  stack = (args, arg_bottom)
  ----
  ret = undef
  global set is processed.
*/
void whole_function_set();

/*
  stack = (args, arg_bottom)
  ----
  in variable set:
    sets value locally

  in function definition:
    immediately the function is set to the variable.
*/
void whole_function_define();

/*
  stack = (args, arg_bottom)
  ----
  code = (arg0, arg1, ...)
*/
void whole_function_begin();

/*
  stack = (args, arg_bottom)
  ----
  ret = extracted expr
*/
void whole_function_quasiquote();

void eval();

#endif // EVAL_HH
