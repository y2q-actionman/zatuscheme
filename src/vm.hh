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
  Env* frame() const { return frames_.back(); }
  void frame_replace(Env* e){ frames_.back() = e; }

  Lisp_ptr find(Symbol*);
  void set(Symbol*, Lisp_ptr);
  void local_set(Symbol*, Lisp_ptr);

  SymTable& symtable(){ return *symtable_; }

public:
  std::deque<Lisp_ptr> code;
  std::deque<Lisp_ptr> stack;
  Lisp_ptr return_value[return_value_max];

private:
  std::deque<Env*> frames_;
  std::shared_ptr<SymTable> symtable_;
};

extern VM vm;

#include "vm.i.hh"

#endif //VM_HH
