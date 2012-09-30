#include "builtin_util.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include <iterator>

void procedure_list(){
  VM.return_value = stack_to_list<false>(VM.stack);
}

void procedure_list_star(){
  VM.return_value = stack_to_list<true>(VM.stack);
}

void procedure_vector(){
  auto v = new Vector;
  stack_to_vector(VM.stack, *v);
  VM.return_value = v;
}

Lisp_ptr pick_args_1(){
  auto tmp = pick_args<1>();
  return tmp[0];
}

int clean_args(){
  int ret = 0;

  while(!VM.stack.empty()
        && VM.stack.top().tag() != Ptr_tag::vm_op){
    VM.stack.pop();
    ++ret;
  }

  if(!VM.stack.empty()
     && VM.stack.top().tag() == Ptr_tag::vm_op){
    VM.stack.pop();
  }

  return ret;
}
