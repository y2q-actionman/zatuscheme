#include "builtin_procedure.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "eval.hh"
#include "util.hh"
#include "delay.hh"

using namespace std;

Lisp_ptr type_check_procedure(){
  ZsArgs args;
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
  ZsArgs args;

  auto d = args[0].get<Delay*>();
  if(!d){
    return args[0];
  }
  
  if(d->forced()){
    return d->get();
  }

  // evaluates Delay's contents
  args.~ZsArgs();

  auto oldenv = vm.frame();
  vm.set_frame(d->env());
  vm.stack.push_back(d);
  vm.code.insert(vm.code.end(),
                 {vm_op_force, oldenv, vm_op_leave_frame, d->get()});
  return {};
}

Lisp_ptr proc_values(){
  vm.return_value.clear();
  stack_to_vector(vm.stack, vm.return_value);
  return {};
}

Lisp_ptr call_with_values(){
  Lisp_ptr procs[2];
  {
    ZsArgs args;

    if(!is_procedure(args[0])){
      throw zs_error("call-with-values error: first arg is not procedure (%s)\n",
                          stringify(args[0].tag()));
    }

    auto info = get_procinfo(args[0]);
    if(info->required_args != 0){
      throw zs_error("call-with-values error: first arg takes 1 or more args (%d)\n",
                          info->required_args);
    }    

    if(!is_procedure(args[1])){
      throw zs_error("call-with-values error: second arg is not procedure (%s)\n",
                          stringify(args[1].tag()));
    }
    
    std::copy(args.begin(), args.end(), procs);
  }

  // second proc call
  vm.code.insert(vm.code.end(), {procs[1], vm_op_proc_enter, vm_op_move_values});

  // first proc, calling with zero args.
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  proc_enter_entrypoint(procs[0]); // direct jump to proc_enter()
  return {};
}

Lisp_ptr call_cc(){
  Lisp_ptr proc;
  {
    ZsArgs args;

    if(!is_procedure(args[0])){
      throw zs_error("call/cc error: first arg is not procedure (%s)\n",
                          stringify(args[0].tag()));
    }

    auto info = get_procinfo(args[0]);
    if(info->required_args != 1){
      throw zs_error("call/cc error: first arg mush take 1 arg (%d)\n",
                          info->required_args);
    }
    proc = args[0];
  }

  auto cont = new Continuation(vm);
  vm.stack.insert(vm.stack.end(), {cont, {Ptr_tag::vm_argcount, 1}});
  proc_enter_entrypoint(proc); // direct jump to proc_enter()
  return {};
}

Lisp_ptr dynamic_wind(){
  Lisp_ptr procs[3];
  {
    ZsArgs args;
    auto procs_i = begin(procs);

    for(auto p : args){
      if(!is_procedure(p)){
        throw zs_error("error: dynamic-wind: arg is not procedure (%s)\n",
                            stringify(p.tag()));
      }

      auto info = get_procinfo(p);
      if(info->required_args != 0){
        throw zs_error("error: dynamic-wind: first arg mush take 0 arg (%d)\n",
                            info->required_args);
      }

      *procs_i = p;
      ++procs_i;
    }
  }

  vm.extent.push_back({procs[0], procs[1], procs[2]});
  vm.code.push_back(vm_op_leave_winding);

  // third proc call
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.push_back(procs[2]);
  vm.code.push_back(vm_op_save_values_and_enter);

  // second proc call
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.push_back(procs[1]);
  vm.code.push_back(vm_op_proc_enter);

  // first proc, calling with zero args.
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  proc_enter_entrypoint(procs[0]); // direct jump to proc_enter()
  return {};
}
