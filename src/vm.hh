#ifndef VM_HH
#define VM_HH

#include <vector>
#include <stack>
#include <deque>
#include <memory>
#include <ostream>

#include "lisp_ptr.hh"
#include "cons.hh"
#include "symbol.hh"
#include "env.hh"

class VM {
public:
  struct winding {
    Lisp_ptr before;
    Lisp_ptr thunk;
    Lisp_ptr after;
  };

  VM();
  VM(const VM&);
  VM(VM&&) = delete;

  ~VM();

  VM& operator=(const VM&);
  VM& operator=(VM&&) = delete;

  void enter_frame(Env*);
  void leave_frame();
  Env* frame() const { return frames_.back(); }

  Lisp_ptr find(Symbol*);
  void set(Symbol*, Lisp_ptr);
  void local_set(Symbol*, Lisp_ptr);

  SymTable& symtable(){ return *symtable_; }

  friend std::ostream& operator<<(std::ostream&, const VM&);

public:
  std::deque<Lisp_ptr> code;
  std::deque<Lisp_ptr> stack;
  std::vector<Lisp_ptr> return_value;

  std::vector<winding> extent;

private:
  std::deque<Env*> frames_;
  std::shared_ptr<SymTable> symtable_;
};

extern VM vm;

#include "vm.i.hh"

#endif //VM_HH
