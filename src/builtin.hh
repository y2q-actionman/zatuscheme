#ifndef BUILTIN_HH
#define BUILTIN_HH

#include "lisp_ptr.hh"
#include "vm.hh"

void install_builtin();

// used in some builtins. exported for test..
bool eq(Lisp_ptr, Lisp_ptr);
bool eql(Lisp_ptr, Lisp_ptr);

#endif // BUILTIN_HH
