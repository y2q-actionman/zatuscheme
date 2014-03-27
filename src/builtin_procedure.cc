#include <iterator>

#include "builtin_procedure.hh"
#include "cons_util.hh"
#include "eval.hh"
#include "lisp_ptr.hh"
#include "procedure.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace builtin {

Lisp_ptr procedurep(ZsArgs args){
  return Lisp_ptr{is_procedure(args[0])};
}

Lisp_ptr apply(ZsArgs args){
  check_procedure_type(args[0]);

  auto proc = args[0];
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
  vm.stack.push_back(VMArgcount{argc});
  vm.code.insert(vm.code.end(), {proc, vm_op_proc_enter});
  return {};
}

Lisp_ptr values(ZsArgs args){
  vm.return_value.assign(begin(args), end(args));
  return {};
}

Lisp_ptr call_with_values(ZsArgs args){
  Lisp_ptr procs[2];

  for(int i = 0; i < 2; ++i){
    check_procedure_type(args[i]);
    procs[i] = args[i];
  }

  args.cleanup();

  // second proc call
  vm.code.insert(vm.code.end(), {procs[1], vm_op_proc_enter, vm_op_move_values});

  // first proc, calling with zero args.
  vm.stack.push_back(VMArgcount{0});
  vm.code.insert(vm.code.end(), {procs[0], vm_op_proc_enter});
  return {};
}

Lisp_ptr call_cc(ZsArgs args){
  check_procedure_type(args[0]);

  auto proc = args[0];
  args.cleanup();

  vm.stack.insert(vm.stack.end(), {zs_new<VM>(vm), VMArgcount{1}});
  vm.code.insert(vm.code.end(), {proc, vm_op_proc_enter});
  return {};
}

Lisp_ptr internal_push_winding(ZsArgs args){
  Lisp_ptr procs[3];

  for(int i = 0; i < 3; ++i){
    check_procedure_type(args[i]);
    procs[i] = args[i];
  }

  vm.extent.push_back({procs[0], procs[1], procs[2]});
  return {};
}

Lisp_ptr internal_pop_winding(ZsArgs){
  if(!vm.extent.empty()){
    vm.extent.pop_back();
  }
  return {};
}

} // namespace builtin

