#ifndef BUILTIN_HH
#define BUILTIN_HH

#include "lisp_ptr.hh"
#include "vm.hh"

void plus_2();
void zs_list();
template <Ptr_tag p> void type_check_pred();
bool eq(Lisp_ptr, Lisp_ptr);
bool eql(Lisp_ptr, Lisp_ptr);

void install_builtin();

#endif // BUILTIN_HH
