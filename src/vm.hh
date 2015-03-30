#ifndef VM_HH
#define VM_HH

#include <iosfwd>
#include <memory>
#include <vector>

#include "lisp_ptr.hh"
#include "symbol.hh"

namespace zs {

class VM {
public:
  struct Winding {
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

  Lisp_ptr return_value_1();

  friend std::ostream& operator<<(std::ostream&, const VM&);

public:
  std::vector<Lisp_ptr> code;
  std::vector<Lisp_ptr> stack;
  std::vector<Lisp_ptr> return_value;

  std::vector<Winding> extent;
  std::shared_ptr<SymTable> symtable;
  Env* frame;
  std::vector<Lisp_ptr> exception_handler;

  Lisp_ptr name;
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

  
  const Lisp_ptr& operator[](int i) const
  { return stack_iter_s_[i]; }

  int size() const
  { return size_; }

  IterType begin() const
  { return stack_iter_s_; }

  IterType end() const
  { return stack_iter_s_ + size(); }

  void cleanup();

private:
  void invalidate();

  int size_; // not containing last vm_argcount
  IterType stack_iter_s_;
};

ZsArgs::IterType begin(const ZsArgs&);
ZsArgs::IterType end(const ZsArgs&);

} // namespace zs

#include "vm.i.hh"

#endif //VM_HH
