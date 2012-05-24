#ifndef EVAL_HH
#define EVAL_HH

#include "lisp_ptr.hh"
#include "vm.hh"

Lisp_ptr eval(Lisp_ptr);

void enclose(Lisp_ptr, Env*);

#endif // EVAL_HH
