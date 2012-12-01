#ifndef BUILTIN_UTIL_HH
#define BUILTIN_UTIL_HH

#include <array>
#include "lisp_ptr.hh"
#include "vm.hh"
#include "procedure.hh"
#include "util.hh"

template<bool dot_list, typename StackT>
Lisp_ptr stack_to_list(StackT&);

template<typename StackT, typename VectorT>
void stack_to_vector(StackT&, VectorT&);

// VM::stack accessor
template<int i>
std::array<Lisp_ptr, i> pick_args();

Lisp_ptr pick_args_1();

class ZsArgs{
public:
  explicit ZsArgs();
  explicit ZsArgs(int argc);
  ZsArgs(const ZsArgs&) = delete;
  ZsArgs(ZsArgs&&) = delete;

  ~ZsArgs();

  ZsArgs& operator=(const ZsArgs&) = delete;
  ZsArgs& operator=(ZsArgs&&) = delete;

  
  Lisp_ptr& operator[](int i){ return *(stack_iter_s_ + i); }
  int size(){ return stack_iter_e_ - stack_iter_s_ - 1; }

  decltype(vm.stack.end()) begin(){ return stack_iter_s_; }
  decltype(vm.stack.end()) end(){ return stack_iter_e_ - 1; }

private:
  decltype(vm.stack.end()) stack_iter_s_;
  decltype(vm.stack.end()) stack_iter_e_; // contains last vm_argcount
};

// builtin type checking
zs_error builtin_type_check_failed(const char*, Ptr_tag, Lisp_ptr);

zs_error builtin_variadic_argcount_failed(const char*, int);


// builtin func struct
struct BuiltinFunc {
  const char* name;
  const Procedure::NProcedure func;

  constexpr BuiltinFunc(const char* n, const Procedure::NProcedure& f)
    : name(n), func(f){};
};

// type check predicate
template <Ptr_tag p>
Lisp_ptr type_check_pred(){
  auto arg = pick_args_1();
  return Lisp_ptr{arg.tag() == p};
}

#include "builtin_util.i.hh"

#endif //BUILTIN_UTIL_HH
