#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
class Env;
class Stack;

Lisp_ptr eval(Lisp_ptr, Env&, Stack&);

#endif // EVAL_HH
