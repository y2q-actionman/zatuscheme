#ifndef BUILTIN_UTIL_I_HH
#define BUILTIN_UTIL_I_HH

#ifndef BUILTIN_UTIL_HH
#error "Please include via parent file"
#endif

#include <stack>
#include <vector>
#include <array>
#include <cstdio>

#include "lisp_ptr.hh"
#include "cons.hh"
#include "vm.hh"

template<typename StackT>
Lisp_ptr stack_to_list(StackT& st, bool dot_list){
  Cons* c = new Cons;
  Cons* prev_c = c;
  Lisp_ptr ret = Lisp_ptr{c};

  while(1){
    c->rplaca(st.top());
    st.pop();

    if(st.top().tag() == Ptr_tag::vm_op){
      st.pop();
      break;
    }

    Cons* newc = new Cons;
    c->rplacd(Lisp_ptr(newc));
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
              fprintf(stderr, "eval warning: dot list has read as proper list. (in %s)\n",
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
  fprintf(stderr, "eval error: stack corruption.\n");
  return -1;
}

}

template<typename Iter>
int pick_args(Iter b, Iter e){
  int ret = 0;

  for(Iter i = b; i != e; ++i){
    if(VM.stack().empty())
      return pick_args_detail::fail();
    *i = VM.stack().top();
    VM.stack().pop();
    ++ret;
  }

  if(VM.stack().empty()
     || VM.stack().top().tag() != Ptr_tag::vm_op)
    return pick_args_detail::fail();

  VM.stack().pop();

  return ret;
}


template<int i>
std::array<Lisp_ptr, i> pick_args(){
  auto ret = std::array<Lisp_ptr, i>();

  auto fillcnt = pick_args(ret.rbegin(), ret.rend());
  if(fillcnt < 0){
    ret.fill({});
  }
    
  return ret;
}

#endif //BUILTIN_UTIL_I_HH
