#include "builtin_util.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include <iterator>

void procedure_list(){
  vm.return_value[0] = stack_to_list<false>(vm.stack);
}

void procedure_list_star(){
  vm.return_value[0] = stack_to_list<true>(vm.stack);
}

void procedure_vector(){
  auto v = new Vector;
  stack_to_vector(vm.stack, *v);
  vm.return_value[0] = v;
}

Lisp_ptr pick_args_1(){
  auto tmp = pick_args<1>();
  return tmp[0];
}

void builtin_type_check_failed(const char* func_name, Ptr_tag tag, Lisp_ptr p){
  fprintf(zs::err, "native func: %s: arg is not %s! (%s)\n",
          func_name, stringify(tag), stringify(p.tag()));
  vm.return_value[0] = {};
}
