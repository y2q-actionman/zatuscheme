#ifndef BUILTIN_UTIL_HH
#define BUILTIN_UTIL_HH

#include <array>
#include "lisp_ptr.hh"
#include "vm.hh"
#include "procedure.hh"

class ZsArgs{
public:
  typedef decltype(vm.stack.end()) IterType;

  explicit ZsArgs();
  explicit ZsArgs(int);
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

private:
  void invalidate();
  bool valid() const;

  int size_; // not containing last vm_argcount
  IterType stack_iter_s_;
};


// type check predicate
template <Ptr_tag p>
Lisp_ptr type_check_pred(){
  ZsArgs args;
  return Lisp_ptr{args[0].tag() == p};
}

#include "builtin_util.i.hh"

#endif //BUILTIN_UTIL_HH
