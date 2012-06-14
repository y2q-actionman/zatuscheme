#ifndef BUILTIN_HH
#define BUILTIN_HH

#include "lisp_ptr.hh"
#include "vm.hh"

void stack_to_list(bool dot_list);
void stack_to_vector();
void eq();
void eql();

void install_builtin();

#endif // BUILTIN_HH
