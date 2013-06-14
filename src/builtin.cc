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
#include "zs_scm_include.hh"
#include "util.hh"

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

  auto oldenv = vm.frame;
  vm.frame = env;
  vm.return_value = {oldenv, vm_op_leave_frame, args[0]};
  return {};
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

static const char* builtin_syntax_str =
#include "builtin_syntax.scm"
;

static const BuiltinNProc builtin_funcs[] = {
#include "builtin.defs.hh"
#include "builtin_boolean.defs.hh"
#include "builtin_char.defs.hh"
#include "builtin_cons.defs.hh"
#include "builtin_equal.defs.hh"
#include "builtin_numeric.defs.hh"
#include "builtin_port.defs.hh"
#include "builtin_procedure.defs.hh"
#include "builtin_string.defs.hh"
#include "builtin_symbol.defs.hh"
#include "builtin_vector.defs.hh"
};

static const char* builtin_str =
#include "builtin.scm"
#include "builtin_boolean.scm"
#include "builtin_char.scm"
#include "builtin_cons.scm"
#include "builtin_numeric.scm"
#include "builtin_port.scm"
#include "builtin_procedure.scm"
#include "builtin_string.scm"
#include "builtin_vector.scm"
;

static const BuiltinNProc builtin_extra_funcs[] = {
#include "builtin_extra.defs.hh"
#include "builtin_srfi.defs.hh"
};

static const char* builtin_extra_str =
#include "builtin_extra.scm"
#include "builtin_srfi.scm"
;


void install_builtin(){
  static constexpr auto install_native = [](const BuiltinNProc& bf){
    vm.frame->local_set(intern(*vm.symtable, bf.name), {&bf.func});
  };    
  static constexpr auto install_string = [](const char* s){
    istringstream iss{s};
    load_internal(iss);
  };    
  static constexpr auto install_symbol = [](const char* name, Lisp_ptr value){
    vm.frame->local_set(intern(*vm.symtable, name), value);
  };

  assert(vm.code.empty() && vm.stack.empty());
  assert(!vm.frame);

  // symtable
  assert(!vm.symtable);
  vm.symtable.reset(new SymTable());

  // null-environment
  vm.frame = zs_new<Env>(nullptr);
  for(auto& i : builtin_syntax_funcs) install_native(i);
  install_string(builtin_syntax_str);
  eval();
  assert(vm.code.empty() && vm.stack.empty());

  auto null_env = vm.frame;

  // r5rs-environment
  vm.frame = vm.frame->push();
  for(auto& i : builtin_funcs) install_native(i);
  install_string(builtin_str);
  install_symbol(EXPAND_STRINGIFY(CURRENT_INPUT_PORT_SYMNAME), &std::cin);
  install_symbol(EXPAND_STRINGIFY(CURRENT_OUTPUT_PORT_SYMNAME), &std::cout);
  install_symbol(EXPAND_STRINGIFY(NULL_ENV_SYMNAME), null_env);
  install_symbol(EXPAND_STRINGIFY(R5RS_ENV_SYMNAME), vm.frame);
  eval();
  assert(vm.code.empty() && vm.stack.empty());

  // interaction-environment
  auto i_env = vm.frame->push();
  install_symbol(EXPAND_STRINGIFY(INTERACTION_ENV_SYMNAME), i_env);

  vm.frame = i_env;
  for(auto& i : builtin_extra_funcs) install_native(i);
  install_string(builtin_extra_str);
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
    throw zs_error_arg1(nullptr,
                        printf_string("internal error: not registered native function! (%s)", name));
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
