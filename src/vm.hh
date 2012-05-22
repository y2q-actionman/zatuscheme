#ifndef VM_HH
#define VM_HH

#include "env.hh"
#include "stack.hh"
#include "symtable.hh"

struct VM_t {
  SymTable symtable;
  Env env;
  Stack stack;
};

extern VM_t VM;

#endif //VM_HH
