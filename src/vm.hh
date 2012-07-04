#ifndef VM_HH
#define VM_HH

#include <vector>
#include <stack>

#include "lisp_ptr.hh"
#include "cons.hh"
#include "symbol.hh"
#include "symtable.hh"

class VM_t {
  typedef std::stack<Lisp_ptr, std::vector<Lisp_ptr>> stack_t;

public:
  VM_t();

  stack_t& code()
  { return codes_; }

  stack_t& stack()
  { return stack_; }

  Lisp_ptr& return_value()
  { return return_value_; }

  void enter_frame(Lisp_ptr);
  void leave_frame();

  Lisp_ptr frame() const
  { return frame_; }
  
  Lisp_ptr find(Symbol*);
  void set(Symbol*, Lisp_ptr);
  void local_set(Symbol*, Lisp_ptr);

public:
  SymTable symtable;

private:
  stack_t codes_;
  stack_t stack_;
  Lisp_ptr return_value_;
  Lisp_ptr frame_;
  stack_t frame_history_;

  Lisp_ptr traverse(Symbol*, Lisp_ptr);
};

Lisp_ptr push_frame(Lisp_ptr);

extern VM_t VM;

#include "vm.i.hh"

#endif //VM_HH
