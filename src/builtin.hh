#ifndef BUILTIN_HH
#define BUILTIN_HH

#include "lisp_ptr.hh"
#include "vm.hh"

// for internal usage
void list_star();
void make_vector();
void eq();
void eql();

void install_builtin();

#endif // BUILTIN_HH
