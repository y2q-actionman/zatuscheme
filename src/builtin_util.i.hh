#ifndef BUILTIN_UTIL_I_HH
#define BUILTIN_UTIL_I_HH

#ifndef BUILTIN_UTIL_HH
#error "Please include via parent file"
#endif

#include <stack>
#include <vector>
#include <array>
#include <cstdio>
#include <iterator>

#include "util.hh"
#include "lisp_ptr.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "vm.hh"

template<bool dot_list, typename StackT>
Lisp_ptr stack_to_list(StackT& st){
  Lisp_ptr argc = st.back();
  st.pop_back();

  if(argc.get<int>() <= 0){
    return Cons::NIL;
  }

  Lisp_ptr ret;
  GrowList gl;

  auto arg_start = st.end() - argc.get<int>();
  auto arg_end = st.end();
  auto i = arg_start;

  for(i = arg_start; i < arg_end - 1; ++i){
    gl.push(*i);
  }

  if(dot_list){
    ret = gl.extract_with_tail(*i);
  }else{
    gl.push(*i);
    ret = gl.extract();
  }

  st.erase(arg_start, arg_end);

  return ret;
}

template<typename StackT, typename VectorT>
void stack_to_vector(StackT& st, VectorT& v){
  Lisp_ptr argc = st.back();
  st.pop_back();

  auto arg_start = st.end() - argc.get<int>();
  auto arg_end = st.end();

  for(auto i = arg_start; i != arg_end; ++i){
    v.push_back(*i);
  }

  st.erase(arg_start, arg_end);
}

template<typename StackT>
int list_to_stack(const char* opname, Lisp_ptr l, StackT& st){
  std::stack<Lisp_ptr> tmp;
  
  do_list(l,
          [&](Cons* c) -> bool {
            tmp.push(c->car());
            return true;
          },
          [&](Lisp_ptr last_cdr){
            if(!nullp(last_cdr)){
              fprintf(zs::err, "eval warning: dot list has read as proper list. (in %s)\n",
                      opname);
              tmp.push(last_cdr);
            }
          });

  int ret = 0;

  while(!tmp.empty()){
    st.push_back(tmp.top());
    tmp.pop();
    ++ret;
  }

  return ret;
}  

template<int size>
std::array<Lisp_ptr, size> pick_args(){
  Lisp_ptr argc = vm.stack.back();
  vm.stack.pop_back();

  auto ret = std::array<Lisp_ptr, size>();
  if(argc.get<int>() != size){
    ret.fill({});
    return ret;
  }    

  for(int i = 0; i < size; ++i){
    ret[i] = vm.stack[vm.stack.size() - size + i];
  }
  vm.stack.erase(vm.stack.end() - size, vm.stack.end());

  return ret;
}


// class ArgAccessor
inline
Lisp_ptr& ArgAccessor::operator[](int i){
  return *(stack_iter_s_ + i);
}

inline
decltype(vm.stack.end()) ArgAccessor::begin(){
  return stack_iter_s_;
}

inline
decltype(vm.stack.end()) ArgAccessor::end(){
  return stack_iter_e_ - 1;
}

#endif //BUILTIN_UTIL_I_HH
