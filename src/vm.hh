#ifndef VM_HH
#define VM_HH

#include <memory>
#include <vector>
#include <iosfwd>

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

  void set_frame(Env* e){ frame_ = e; }
  Env* frame() const { return frame_; }

  Lisp_ptr find(Symbol*);
  void set(Symbol*, Lisp_ptr);
  void local_set(Symbol*, Lisp_ptr);

  SymTable& symtable(){ return *symtable_; }

  friend std::ostream& operator<<(std::ostream&, const VM&);

public:
  std::vector<Lisp_ptr> code;
  std::vector<Lisp_ptr> stack;
  std::vector<Lisp_ptr> return_value;

  std::vector<winding> extent;

private:
  Env* frame_;
  std::shared_ptr<SymTable> symtable_;
};

extern VM vm;

#include "vm.i.hh"

#endif //VM_HH
