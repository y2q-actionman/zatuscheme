#include <sstream>
#include <istream>
#include <fstream>
#include <iostream>
#include <cstring>
#include <string>
#include <vector>

#include "builtin.hh"
#include "zs_error.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "reader.hh"
#include "printer.hh"
#include "vm.hh"
#include "zs_memory.hh"

#include "builtin_boolean.hh"
#include "builtin_char.hh"
#include "builtin_cons.hh"
#include "builtin_equal.hh"
#include "builtin_extra.hh"
#include "builtin_numeric.hh"
#include "builtin_port.hh"
#include "builtin_procedure.hh"
#include "builtin_srfi.hh"
#include "builtin_string.hh"
#include "builtin_symbol.hh"
#include "builtin_syntax.hh"
#include "builtin_vector.hh"

using namespace std;
using namespace proc_flag;

namespace {

static const char null_env_symname[] = "null-env-value";
static const char r5rs_env_symname[] = "r5rs-env-value";
static const char interaction_env_symname[] = "interaction-env-value";

Lisp_ptr env_pick_2(Lisp_ptr arg1, const char* envname){
  if(arg1.tag() != Ptr_tag::integer){
    throw builtin_type_check_failed(nullptr, Ptr_tag::integer, arg1);
  }

  auto ver = arg1.get<int>();
  if(ver != 5){
    throw zs_error_arg1(nullptr, "passed number is not 5", {arg1});
  }

  return vm.frame()->find(intern(*vm.symtable, envname));
}

} //namespace

void load_internal(std::istream& ss){
  vector<Lisp_ptr> tmpv;

  while(ss){
    auto form = read(ss);
    if(!form || eof_object_p(form)){
      break;
    }
    tmpv.push_back(form);
  }

  vm.code.insert(vm.code.end(), tmpv.rbegin(), tmpv.rend());
}

namespace builtin {

Lisp_ptr eval(ZsArgs args){
  auto env = args[1].get<Env*>();
  if(!env){
    throw builtin_type_check_failed(nullptr, Ptr_tag::env, args[1]);
  }

  auto oldenv = vm.frame();
  vm.set_frame(env);
  vm.return_value = {oldenv, vm_op_leave_frame, args[0]};
  return {};
}

Lisp_ptr env_r5rs(ZsArgs args){
  return env_pick_2(args[0], r5rs_env_symname);
}

Lisp_ptr env_null(ZsArgs args){
  return env_pick_2(args[0], null_env_symname);
}

Lisp_ptr env_interactive(ZsArgs){
  return vm.frame()->find(intern(*vm.symtable, interaction_env_symname));
}

Lisp_ptr load(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  ifstream ifs{*str};
  load_internal(ifs);
  return Lisp_ptr{true};
}

} // namespace builtin

static const BuiltinNProc builtin_syntax_funcs[] = {
#include "builtin_syntax.defs.hh"
};

static const char* builtin_syntax_strs[] = {
#include "builtin_syntax.strs.hh"
};

static const BuiltinNProc builtin_funcs[] = {
#include "builtin.defs.hh"
#include "builtin_boolean.defs.hh"
#include "builtin_char.defs.hh"
#include "builtin_cons.defs.hh"
#include "builtin_equal.defs.hh"
#include "builtin_numeric.defs.hh"
#include "builtin_port.defs.hh"
#include "builtin_procedure.defs.hh"
#include "builtin_srfi.defs.hh"
#include "builtin_string.defs.hh"
#include "builtin_symbol.defs.hh"
#include "builtin_vector.defs.hh"
};

static const char* builtin_strs[] = {
#include "builtin_boolean.strs.hh"
#include "builtin_cons.strs.hh"
#include "builtin_numeric.strs.hh"
#include "builtin_procedure.strs.hh"
#include "builtin_port.strs.hh"
};

static const BuiltinNProc builtin_extra_funcs[] = {
#include "builtin_extra.defs.hh"
};

static const char* builtin_extra_strs[] = {
#include "builtin_extra.strs.hh"
};


void install_builtin(){
  static constexpr auto install_native = [](const BuiltinNProc& bf){
    vm.frame()->local_set(intern(*vm.symtable, bf.name), {&bf.func});
  };    
  static constexpr auto install_string = [](const char* s){
    istringstream iss{s};
    load_internal(iss);
  };    
  static constexpr auto install_symbol = [](const char* name, Lisp_ptr value){
    vm.frame()->local_set(intern(*vm.symtable, name), value);
  };

  // symtable
  assert(!vm.symtable);
  vm.symtable.reset(new SymTable());

  // null-environment
  assert(vm.code.empty() && vm.stack.empty());
  assert(!vm.frame());
  vm.set_frame(zs_new<Env>(nullptr));
  for(auto& i : builtin_syntax_funcs) install_native(i);
  for(auto i : builtin_syntax_strs) install_string(i);
  eval();
  auto null_env = vm.frame();

  // r5rs-environment
  assert(vm.code.empty() && vm.stack.empty());
  vm.set_frame(vm.frame()->push());
  for(auto& i : builtin_funcs) install_native(i);
  for(auto i : builtin_strs) install_string(i);
  install_symbol(CURRENT_INPUT_PORT_SYMNAME, &std::cin);
  install_symbol(CURRENT_OUTPUT_PORT_SYMNAME, &std::cout);
  install_symbol(null_env_symname, null_env);
  install_symbol(r5rs_env_symname, vm.frame());
  eval();

  // interaction-environment
  assert(vm.code.empty() && vm.stack.empty());
  vm.set_frame(vm.frame()->push());
  for(auto& i : builtin_extra_funcs) install_native(i);
  for(auto i : builtin_extra_strs) install_string(i);
  install_symbol(interaction_env_symname, vm.frame());
  eval();
}


template<typename Fun>
static
const BuiltinNProc* find_builtin_nproc_internal(Fun fun){
  for(auto& i : builtin_syntax_funcs) if(fun(i)) return &i;
  for(auto& i : builtin_funcs)        if(fun(i)) return &i;
  for(auto& i : builtin_extra_funcs)  if(fun(i)) return &i;
  return nullptr;
}

const NProcedure* find_builtin_nproc(const char* name){
  if(auto p = find_builtin_nproc_internal
     ([=](const BuiltinNProc& bf){ return strcmp(name, bf.name) == 0; })){
    return &(p->func);
  }else{
    throw zs_error(printf_string("internal error: native function '%s' is not registered!", name));
  }
}

const char* find_builtin_nproc_name(const NProcedure* nproc){
  if(auto p = find_builtin_nproc_internal
     ([=](const BuiltinNProc& bf){ return nproc == &bf.func; })){
    return p->name;
  }else{
    return "(unknown native procedure)";
  }
}
