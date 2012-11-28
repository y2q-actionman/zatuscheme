#include "builtin_util.hh"

Lisp_ptr pick_args_1(){
  auto tmp = pick_args<1>();
  return tmp[0];
}

zs_error builtin_type_check_failed(const char* func_name, Ptr_tag tag, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(tag), stringify(p.tag()));
}

zs_error builtin_variadic_argcount_failed(const char* name, int argc){
  return make_zs_error("native func: %s: %d or more args.\n", name, argc+1);
}

