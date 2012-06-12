#ifndef BUILTIN_HH
#define BUILTIN_HH

#include "lisp_ptr.hh"
#include "vm.hh"

void plus_2();
void stack_to_list(bool dot_list);
template <Ptr_tag p> void type_check_pred();
void eq();
void eql();

void install_builtin();

#endif // BUILTIN_HH
