#ifndef VM_HH
#define VM_HH

#include "env.hh"
#include "stack.hh"
#include "symtable.hh"

class Symbol;

class VM_t {
public:
  SymTable symtable;

private:
  Env env;
  Stack stack;

public:
  Lisp_ptr find(Symbol*) const;
  Lisp_ptr local_set(Symbol*, Lisp_ptr);
  Lisp_ptr global_set(Symbol*, Lisp_ptr);

  void push(Symbol*, Lisp_ptr);
  void pop(int);

  Lisp_ptr at(int) const;
  int size() const;
};

extern VM_t VM;

#endif //VM_HH
