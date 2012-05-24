#ifndef VM_HH
#define VM_HH

#include <vector>

#include "lisp_ptr.hh"
#include "symtable.hh"

class Symbol;

class VM_t {
public:
  VM_t();

  void enter_frame(Env*);
  void leave_frame();

  int frame_depth() const
  { return frame_depth_; }

  Lisp_ptr frame_head() const
  { return frames_; }
  
  Lisp_ptr find(Symbol*) const;
  void set(Symbol*, Lisp_ptr);

  void arg_push(Lisp_ptr);
  Lisp_ptr arg_get(int) const;
  void arg_clear();

public:
  SymTable symtable;

private:
  Lisp_ptr frames_;
  int frame_depth_;
  std::vector<Lisp_ptr> args_;
};

extern VM_t VM;

#endif //VM_HH
