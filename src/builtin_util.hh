#ifndef BUILTIN_UTIL_HH
#define BUILTIN_UTIL_HH

#include <array>
#include "lisp_ptr.hh"
#include "vm.hh"
#include "procedure.hh"

template<bool dot_list, typename StackT>
Lisp_ptr stack_to_list(StackT&);

template<typename StackT, typename VectorT>
void stack_to_vector(StackT&, VectorT&);

template<typename StackT>
int list_to_stack(const char*, Lisp_ptr, StackT&);


template<int i>
std::array<Lisp_ptr, i> pick_args();

Lisp_ptr pick_args_1();


// builtin type checking
void builtin_type_check_failed(const char*, Ptr_tag, Lisp_ptr);

void builtin_variadic_argcount_failed(const char*, int);


// builtin func struct
struct BuiltinFunc {
  const char* name;
  const Procedure::NProcedure func;

  constexpr BuiltinFunc(const char* n, const Procedure::NProcedure& f)
    : name(n), func(f){};
};

// type check predicate
template <Ptr_tag p>
void type_check_pred(){
  auto arg = pick_args_1();
  vm.return_value[0] = Lisp_ptr{arg.tag() == p};
}

#include "builtin_util.i.hh"

#endif //BUILTIN_UTIL_HH
