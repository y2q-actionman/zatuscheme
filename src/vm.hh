#ifndef VM_HH
#define VM_HH

#include <vector>
#include <stack>
#include <deque>
#include <memory>

#include "lisp_ptr.hh"
#include "cons.hh"
#include "symbol.hh"
#include "env.hh"

class VM {
  typedef std::stack<Lisp_ptr, std::vector<Lisp_ptr>> stack_t;

public:
  static constexpr int return_value_max = 32;

  VM();
  VM(const VM&) = delete;
  VM(VM&&) = delete;

  ~VM();

  VM& operator=(const VM&) = delete;
  VM& operator=(VM&&) = delete;

  void enter_frame(Env*);
  void leave_frame();

  Lisp_ptr find(Symbol*);
  void set(Symbol*, Lisp_ptr);
  void local_set(Symbol*, Lisp_ptr);

  SymTable& symtable(){
    return *symtable_;
  }

public:
  stack_t code;
  stack_t stack;
  Lisp_ptr return_value[return_value_max];
  Env* frame;

private:
  std::deque<Env*> frame_history_;
  std::shared_ptr<SymTable> symtable_;
};

extern VM vm;

#include "vm.i.hh"

#endif //VM_HH
