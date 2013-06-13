#include <algorithm>

#include "builtin_procedure.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "procedure.hh"
#include "eval.hh"
#include "util.hh"
#include "zs_error.hh"
#include "cons_util.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

zs_error procedure_type_check_failed(Lisp_ptr p){
  return zs_error_arg1(nullptr, "arg is not procedure!", {p});
}

} // namespace

namespace builtin {

Lisp_ptr procedurep(ZsArgs args){
  return Lisp_ptr{is_procedure(args[0].tag())};
}

Lisp_ptr apply(ZsArgs args){
  auto proc = args[0];
  if(!is_procedure(proc.tag())){
    throw procedure_type_check_failed(proc);
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
  vm.code.insert(vm.code.end(), {proc, vm_op_proc_enter});
  return {};
}

Lisp_ptr values(ZsArgs args){
  vm.return_value.assign(begin(args), end(args));
  return {};
}

Lisp_ptr call_with_values(ZsArgs args){
  Lisp_ptr procs[2];

  if(!is_procedure(args[0].tag())){
    throw procedure_type_check_failed(args[0]);
  }

  auto info = get_procinfo(args[0]);
  if(info->required_args != 0){
    throw builtin_argcount_failed(nullptr, info->required_args, info->max_args, 0);
  }    

  if(!is_procedure(args[1].tag())){
    throw procedure_type_check_failed(args[1]);
  }
    
  std::copy(args.begin(), args.end(), procs);

  args.cleanup();

  // second proc call
  vm.code.insert(vm.code.end(), {procs[1], vm_op_proc_enter, vm_op_move_values});

  // first proc, calling with zero args.
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.insert(vm.code.end(), {procs[0], vm_op_proc_enter});
  return {};
}

Lisp_ptr call_cc(ZsArgs args){
  Lisp_ptr proc;

  if(!is_procedure(args[0].tag())){
    throw procedure_type_check_failed(args[0]);
  }

  auto info = get_procinfo(args[0]);
  if(!(info->required_args <= 1 && 1 <= info->max_args)){
    throw builtin_argcount_failed(nullptr, info->required_args, info->max_args, 1);
  }

  proc = args[0];

  args.cleanup();

  auto cont = zs_new<Continuation>(vm);
  vm.stack.insert(vm.stack.end(), {cont, {Ptr_tag::vm_argcount, 1}});
  vm.code.insert(vm.code.end(), {proc, vm_op_proc_enter});
  return {};
}

Lisp_ptr dynamic_wind(ZsArgs args){
  for(auto p : args){
    if(!is_procedure(p.tag())){
      throw procedure_type_check_failed(p);
    }

    auto info = get_procinfo(p);
    if(info->required_args != 0){
      throw builtin_argcount_failed(nullptr, info->required_args, info->max_args, 0);
    }
  }

  Lisp_ptr procs[3];
  std::copy(begin(args), end(args), begin(procs));
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
  vm.code.insert(vm.code.end(), {procs[0], vm_op_proc_enter});
  return {};
}

} // namespace builtin
