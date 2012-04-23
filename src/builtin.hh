#ifndef BUILTIN_HH
#define BUILTIN_HH

class Env;
class SymTable;
class Lisp_ptr;

void install_builtin(Env&, SymTable&);

// used in some builtins. exported for test..
bool eq(Lisp_ptr, Lisp_ptr);
bool eql(Lisp_ptr, Lisp_ptr);

#endif // BUILTIN_HH
