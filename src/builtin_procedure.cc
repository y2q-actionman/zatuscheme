#include "builtin_procedure.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "eval.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

Lisp_ptr type_check_procedure(){
  ZsArgs args{1};
  return Lisp_ptr{is_procedure(args[0])};
}

Lisp_ptr apply_func(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  if(!is_procedure(args[0])){
    throw zs_error("apply error: first arg is not procedure (%s)\n",
                        stringify(args[0].tag()));
  }

  // simulating function_call()
  int argc = 0;
  for(auto i = std::next(args.begin()), e = args.end(); i != e; ++i){
    if(i->tag() == Ptr_tag::cons){
      for(auto p : *i){
        vm.stack.push_back(p);
        ++argc;
      }
    }else{
      vm.stack.push_back(*i);
      ++argc;
    }
  }
  vm.stack.push_back({Ptr_tag::vm_argcount, argc});

  proc_enter_entrypoint(args[0]); // direct jump to proc_enter()
  return {};
}

Lisp_ptr proc_values(){
  vm.return_value.clear();

  stack_to_vector(vm.stack, vm.return_value);

  if(vm.return_value.empty()){
    vm.return_value.resize(1);
  }

  return {};
}
