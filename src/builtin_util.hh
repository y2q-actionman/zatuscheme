#ifndef BUILTIN_UTIL_HH
#define BUILTIN_UTIL_HH

#include <array>
#include "lisp_ptr.hh"
#include "vm.hh"
#include "procedure.hh"
#include "zs_error.hh"

template<bool dot_list, typename StackT>
Lisp_ptr stack_to_list(StackT&);

template<typename StackT, typename VectorT>
void stack_to_vector(StackT&, VectorT&);

// VM::stack accessor

// template<int i>
// std::array<Lisp_ptr, i> pick_args();

// Lisp_ptr pick_args_1();

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

// builtin type checking
zs_error builtin_type_check_failed(const char*, Ptr_tag, Lisp_ptr);
zs_error builtin_argcount_failed(const char*, int required, int max, int passed);
zs_error builtin_identifier_check_failed(const char*, Lisp_ptr);


// type check predicate
template <Ptr_tag p>
Lisp_ptr type_check_pred(){
  ZsArgs args{1};
  return Lisp_ptr{args[0].tag() == p};
}

#include "builtin_util.i.hh"

#endif //BUILTIN_UTIL_HH
