#include "builtin_util.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include <iterator>

Lisp_ptr pick_args_1(){
  auto tmp = pick_args<1>();
  return tmp[0];
}

ArgAccessor::ArgAccessor(VM& v) : the_vm_(v){
  Lisp_ptr p = the_vm_.stack.back();
  argc_ = p.get<int>();
}

ArgAccessor::ArgAccessor(int request_argc, VM& v) : the_vm_(v){
  // TODO: use delegating constructor
  Lisp_ptr p = the_vm_.stack.back();
  argc_ = p.get<int>();

  if(argc_ != request_argc){
    // throw exception.
  }
}

ArgAccessor::~ArgAccessor(){
  the_vm_.stack.erase(the_vm_.stack.end() - (argc_ + 1),
                      the_vm_.stack.end());
}

void builtin_type_check_failed(const char* func_name, Ptr_tag tag, Lisp_ptr p){
  fprintf(zs::err, "native func: %s: arg is not %s! (%s)\n",
          func_name, stringify(tag), stringify(p.tag()));
  vm.return_value[0] = {};
}

void builtin_variadic_argcount_failed(const char* name, int argc){
  fprintf(zs::err, "native func: %s: %d or more args.\n", name, argc+1);
  vm.return_value[0] = {};
}

