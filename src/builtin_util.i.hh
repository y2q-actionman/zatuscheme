#ifndef BUILTIN_UTIL_I_HH
#define BUILTIN_UTIL_I_HH

#ifndef BUILTIN_UTIL_HH
#error "Please include via parent file"
#endif

#include <stack>
#include <vector>
#include <array>
#include <iterator>
#include <exception>
#include <iostream>

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

// class ZsArgs
ZsArgs::ZsArgs()
  : stack_iter_s_(),
    stack_iter_e_(vm.stack.end()){
  auto argcnt = vm.stack.back().get<int>();
  stack_iter_s_  = stack_iter_e_ - (argcnt + 1);
}

ZsArgs::ZsArgs(int request_argc)
  : stack_iter_s_(),
    stack_iter_e_(vm.stack.end()){
  // TODO: use delegating constructor
  auto argcnt = vm.stack.back().get<int>();
  stack_iter_s_  = stack_iter_e_ - (argcnt + 1);

  if(argcnt != request_argc){
    // throw exception.
  }
}

ZsArgs::~ZsArgs(){
  if(!std::uncaught_exception())
    vm.stack.erase(stack_iter_s_, stack_iter_e_);
}

#endif //BUILTIN_UTIL_I_HH
