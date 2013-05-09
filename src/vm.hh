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

  Lisp_ptr return_value_1()
  { return (return_value.empty()) ? Lisp_ptr{} : return_value[0]; }

  friend std::ostream& operator<<(std::ostream&, const VM&);

public:
  std::vector<Lisp_ptr> code;
  std::vector<Lisp_ptr> stack;
  std::vector<Lisp_ptr> return_value;

  std::vector<winding> extent;
  std::shared_ptr<SymTable> symtable;

private:
  Env* frame_;
};

extern VM vm;

// class for accessing VM's stack from builtin funcs
class ZsArgs{
public:
  typedef decltype(vm.stack.end()) IterType;

  explicit ZsArgs();
  ZsArgs(const ZsArgs&) = delete;
  ZsArgs(ZsArgs&&);

  ~ZsArgs();

  ZsArgs& operator=(const ZsArgs&) = delete;
  ZsArgs& operator=(ZsArgs&&);

  
  Lisp_ptr& operator[](int i) const
  { return *(stack_iter_s_ + i); }

  int size() const
  { return size_; }

  IterType begin() const
  { return stack_iter_s_; }

  IterType end() const
  { return stack_iter_s_ + size(); }

  void cleanup();

private:
  void invalidate();
  bool valid() const;

  int size_; // not containing last vm_argcount
  IterType stack_iter_s_;
};

#endif //VM_HH
