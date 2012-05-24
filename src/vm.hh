#ifndef VM_HH
#define VM_HH

#include <vector>

#include "lisp_ptr.hh"
#include "cons.hh"
#include "symtable.hh"

class Symbol;

class VM_t {
public:
  VM_t();

  void enter_frame(Lisp_ptr);
  void leave_frame();

  Lisp_ptr frame() const
  { return frame_; }
  
  Lisp_ptr find(Symbol*) const;
  void set(Symbol*, Lisp_ptr);

  void arg_push(Lisp_ptr);
  Lisp_ptr arg_get(int) const;
  void arg_clear();

public:
  SymTable symtable;

private:
  Lisp_ptr frame_;
  std::vector<Lisp_ptr> frame_history_;
  std::vector<Lisp_ptr> args_;
};

inline
Lisp_ptr push_frame(Lisp_ptr l){
  return Lisp_ptr(new Cons(Lisp_ptr(new Env), l));
}

extern VM_t VM;

#endif //VM_HH
