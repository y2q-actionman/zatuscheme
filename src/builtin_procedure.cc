#include "builtin_procedure.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "eval.hh"
#include "util.hh"
#include "delay.hh"
#include "builtin_util.hh"
#include "zs_error.hh"
#include "cons_util.hh"

using namespace std;

namespace builtin {

Lisp_ptr procedurep(ZsArgs args){
  return Lisp_ptr{is_procedure(args[0])};
}

Lisp_ptr apply(ZsArgs args){
  auto proc = args[0];
  if(!is_procedure(proc)){
    throw zs_error_arg1("apply", "first arg is not procedure", {proc});
  }

  std::vector<Lisp_ptr> a_args(next(begin(args)), end(args));
  
  args.cleanup();

  // simulating function_call()
  int argc = 0;
  for(auto i : a_args){
    if(i.tag() == Ptr_tag::cons){
      for(auto p : i){
        vm.stack.push_back(p);
        ++argc;
      }
    }else{
      vm.stack.push_back(i);
      ++argc;
    }
  }
  vm.stack.push_back({Ptr_tag::vm_argcount, argc});

  proc_enter_entrypoint(proc); // direct jump to proc_enter()
  return {};
}

Lisp_ptr force(ZsArgs args){
  auto d = args[0].get<Delay*>();
  if(!d){
    return args[0];
  }
  
  if(d->forced()){
    return d->get();
  }

  // evaluates Delay's contents
  args.cleanup();

  auto oldenv = vm.frame();
  vm.set_frame(d->env());
  vm.stack.push_back(d);
  vm.code.insert(vm.code.end(),
                 {vm_op_force, oldenv, vm_op_leave_frame, d->get()});
  return {};
}

Lisp_ptr values(ZsArgs args){
  vm.return_value.assign(begin(args), end(args));
  return {};
}

Lisp_ptr call_with_values(ZsArgs args){
  Lisp_ptr procs[2];

  if(!is_procedure(args[0])){
    throw zs_error_arg1("call-with-values", "first arg is not procedure", {args[0]});
  }

  auto info = get_procinfo(args[0]);
  if(info->required_args != 0){
    throw zs_error_arg1("call-with-values",
                        printf_string("first arg takes 1 or more args (takes %d)",
                                      info->required_args));
  }    

  if(!is_procedure(args[1])){
    throw zs_error_arg1("call-with-values", "second arg is not procedure", {args[1]});
  }
    
  std::copy(args.begin(), args.end(), procs);

  args.cleanup();

  // second proc call
  vm.code.insert(vm.code.end(), {procs[1], vm_op_proc_enter, vm_op_move_values});

  // first proc, calling with zero args.
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  proc_enter_entrypoint(procs[0]); // direct jump to proc_enter()
  return {};
}

Lisp_ptr call_cc(ZsArgs args){
  Lisp_ptr proc;

  if(!is_procedure(args[0])){
    throw zs_error_arg1("call/cc", "first arg is not procedure", {args[0]});
  }

  auto info = get_procinfo(args[0]);
  if(info->required_args != 1){
    throw zs_error_arg1("call/cc",
                        printf_string("first arg mush take 1 arg (takes %d)",
                                      info->required_args));
  }

  proc = args[0];

  args.cleanup();

  auto cont = new Continuation(vm);
  vm.stack.insert(vm.stack.end(), {cont, {Ptr_tag::vm_argcount, 1}});
  proc_enter_entrypoint(proc); // direct jump to proc_enter()
  return {};
}

Lisp_ptr dynamic_wind(ZsArgs args){
  Lisp_ptr procs[3];

  auto procs_i = begin(procs);

  for(auto p : args){
    if(!is_procedure(p)){
      throw zs_error_arg1("dynamic-wind", "arg is not procedure", {p});
    }

    auto info = get_procinfo(p);
    if(info->required_args != 0){
      throw zs_error_arg1("dynamic-wind",
                          printf_string("first arg mush take 0 arg (%d)",
                                        info->required_args));
    }

    *procs_i = p;
    ++procs_i;
  }

  args.cleanup();

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

} // namespace builtin
