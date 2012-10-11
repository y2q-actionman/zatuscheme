#ifndef BUILTIN_UTIL_I_HH
#define BUILTIN_UTIL_I_HH

#ifndef BUILTIN_UTIL_HH
#error "Please include via parent file"
#endif

#include <stack>
#include <vector>
#include <array>
#include <cstdio>

#include "util.hh"
#include "lisp_ptr.hh"
#include "cons.hh"
#include "vm.hh"

template<bool dot_list, typename StackT>
Lisp_ptr stack_to_list(StackT& st){
  Cons* c = new Cons;
  Cons* prev_c = c;
  Lisp_ptr ret = c;

  if(st.top().tag() == Ptr_tag::vm_op){
    st.pop();
    return Cons::NIL;
  }

  while(1){
    c->rplaca(st.top());
    st.pop();

    if(st.top().tag() == Ptr_tag::vm_op){
      st.pop();
      break;
    }

    Cons* newc = new Cons;
    c->rplacd(newc);
    prev_c = c;
    c = newc;
  }

  if(dot_list){
    if(c != prev_c){
      prev_c->rplacd(c->car());
    }else{
      ret = c->car();
    }
    delete c;
  }else{
    c->rplacd(Cons::NIL);
  }

  return ret;
}

template<typename StackT, typename VectorT>
void stack_to_vector(StackT& st, VectorT& v){
  if(st.top().tag() == Ptr_tag::vm_op){
    st.pop();
    return;
  }

  while(1){
    v.push_back(st.top());
    st.pop();

    if(st.top().tag() == Ptr_tag::vm_op){
      st.pop();
      break;
    }
  }
}

template<typename StackT>
int list_to_stack(const char* opname, Lisp_ptr l, StackT& st){
  std::stack<Lisp_ptr, std::vector<Lisp_ptr>> tmp;
  
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
    st.push(tmp.top());
    tmp.pop();
    ++ret;
  }

  return ret;
}  

namespace pick_args_detail {

inline
int fail(){
  fprintf(zs::err, "eval error: stack corruption.\n");
  return -1;
}

}

template<int size>
std::array<Lisp_ptr, size> pick_args(){
  auto ret = std::array<Lisp_ptr, size>();

  for(int i = 0; i < size; ++i){
    if(vm.stack.empty()){
      pick_args_detail::fail();
      ret.fill({});
      return ret;
    }
    ret[i] = vm.stack.top();
    vm.stack.pop();
  }

  if(vm.stack.empty()
     || vm.stack.top().tag() != Ptr_tag::vm_op){
    pick_args_detail::fail();
    ret.fill({});
    return ret;
  }
  vm.stack.pop();

  return ret;
}

#endif //BUILTIN_UTIL_I_HH
