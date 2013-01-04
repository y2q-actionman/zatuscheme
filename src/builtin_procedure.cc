#include "builtin_procedure.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "eval.hh"
#include "util.hh"
#include "delay.hh"

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

Lisp_ptr func_force(){
  auto arg = pick_args_1();     // used because no exceptions are done!
  auto d = arg.get<Delay*>();
  if(!d){
    vm.return_value[0] = arg;
    return {};
  }
  
  if(d->forced()){
    vm.return_value[0] = d->get();
    return {};
  }

  auto oldenv = vm.frame();

  vm.set_frame(d->env());
  vm.stack.push_back(arg);
  vm.code.insert(vm.code.end(),
                 {vm_op_force, oldenv, vm_op_leave_frame, d->get()});
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
