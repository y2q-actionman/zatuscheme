#ifndef BUILTIN_UTIL_HH
#define BUILTIN_UTIL_HH

#include <array>
#include "lisp_ptr.hh"

template<typename StackT>
Lisp_ptr stack_to_list(StackT&, bool);

template<typename StackT, typename VectorT>
void stack_to_vector(StackT&, VectorT&);

template<typename StackT>
int list_to_stack(const char*, Lisp_ptr, StackT&);

template<int i>
std::array<Lisp_ptr, i> pick_args();


// some builtin functions
void procedure_list();
void procedure_list_star();
void procedure_vector();

#include "builtin_util.i.hh"

#endif //BUILTIN_UTIL_HH
