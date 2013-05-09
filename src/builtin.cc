#include <sstream>
#include <istream>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <cstring>
#include <string>

#include "builtin.hh"
#include "zs_error.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "reader.hh"
#include "printer.hh"
#include "vm.hh"

#include "builtin_boolean.hh"
#include "builtin_char.hh"
#include "builtin_cons.hh"
#include "builtin_equal.hh"
#include "builtin_extra.hh"
#include "builtin_numeric.hh"
#include "builtin_port.hh"
#include "builtin_procedure.hh"
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

Lisp_ptr env_pick_2(Lisp_ptr arg1, const char* name){
  if(arg1.tag() != Ptr_tag::integer){
    throw builtin_type_check_failed(name, Ptr_tag::integer, arg1);
  }

  auto ver = arg1.get<int>();
  if(ver != 5){
    throw zs_error_arg1(name, "passed number is not 5", {arg1});
  }

  return vm.frame()->find(intern(vm.symtable(), name));
}

} //namespace

void load_internal(std::istream& ss){
  while(ss){
    auto form = read(ss);
    if(!form){
      if(!ss){
        // cerr << "load error: failed at reading a form. abandoned.\n";
      }
      break;
    }

    if(eof_object_p(form)) break;

    vm.code.push_back(form);
    eval();
    if(!vm.return_value_1()){
      cerr << "load error: failed at evaluating a form. skipped.\n";
      cerr << "form: \n";
      print(cerr, form);
      continue;
    }
  }
}

namespace builtin {

Lisp_ptr eval(ZsArgs args){
  auto env = args[1].get<Env*>();
  if(!env){
    throw builtin_type_check_failed("eval", Ptr_tag::env, args[1]);
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
  return vm.frame()->find(intern(vm.symtable(), interaction_env_symname));
}

Lisp_ptr load(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed("load", Ptr_tag::string, args[0]);
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
  static constexpr auto install_builtin_native = [](const BuiltinNProc& bf){
    vm.frame()->local_set(intern(vm.symtable(), bf.name), {&bf.func});
  };    
  static constexpr auto install_builtin_string = [](const char* s){
    istringstream iss{s};
    load_internal(iss);
  };    
  static constexpr auto install_builtin_symbol = [](const char* name, Lisp_ptr value){
    vm.frame()->local_set(intern(vm.symtable(), name), value);
  };    

  // null-environment
  for_each(std::begin(builtin_syntax_funcs), std::end(builtin_syntax_funcs),
           install_builtin_native);
  for_each(std::begin(builtin_syntax_strs), std::end(builtin_syntax_strs),
           install_builtin_string);
  auto null_env = vm.frame();

  // r5rs-environment
  vm.set_frame(vm.frame()->push());
  for_each(std::begin(builtin_funcs), std::end(builtin_funcs),
           install_builtin_native);
  for_each(std::begin(builtin_strs), std::end(builtin_strs),
           install_builtin_string);
  install_builtin_symbol(CURRENT_INPUT_PORT_SYMNAME, &std::cin);
  install_builtin_symbol(CURRENT_OUTPUT_PORT_SYMNAME, &std::cout);
  install_builtin_symbol(null_env_symname, null_env);
  install_builtin_symbol(r5rs_env_symname, vm.frame());

  // interaction-environment
  vm.set_frame(vm.frame()->push());
  for_each(std::begin(builtin_extra_funcs), std::end(builtin_extra_funcs),
           install_builtin_native);
  for_each(std::begin(builtin_extra_strs), std::end(builtin_extra_strs),
           install_builtin_string);
  install_builtin_symbol(interaction_env_symname, vm.frame());
}

const NProcedure* find_builtin_nproc(const char* name){
  const auto find_func
    = [name](const BuiltinNProc& bf){ return strcmp(name, bf.name) == 0; };

  auto i = find_if(begin(builtin_syntax_funcs), end(builtin_syntax_funcs), find_func);
  if(i != end(builtin_syntax_funcs)) return &(i->func);

  i = find_if(begin(builtin_funcs), end(builtin_funcs), find_func);
  if(i != end(builtin_funcs)) return &(i->func);

  i = find_if(begin(builtin_extra_funcs), end(builtin_extra_funcs), find_func);
  if(i != end(builtin_extra_funcs)) return &(i->func);

  throw zs_error(printf_string("internal error: native function '%s' is not registered!", name));
}

const char* find_builtin_nproc_name(const NProcedure* nproc){
  const auto find_func
    = [nproc](const BuiltinNProc& bf){ return nproc == &bf.func; };

  auto i = find_if(begin(builtin_syntax_funcs), end(builtin_syntax_funcs), find_func);
  if(i != end(builtin_syntax_funcs)) return i->name;

  i = find_if(begin(builtin_funcs), end(builtin_funcs), find_func);
  if(i != end(builtin_funcs)) return i->name;

  i = find_if(begin(builtin_extra_funcs), end(builtin_extra_funcs), find_func);
  if(i != end(builtin_extra_funcs)) return i->name;

  return "(unknown native procedure)";
}
