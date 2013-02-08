#include <sstream>
#include <istream>
#include <iostream>
#include <algorithm>
#include <cstring>

#include "builtin.hh"
#include "zs_error.hh"
#include "number.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
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
using namespace ProcFlag;

namespace {

static const char null_env_symname[] = "null-env-value";
static const char r5rs_env_symname[] = "r5rs-env-value";
static const char interaction_env_symname[] = "interaction-env-value";

static
Lisp_ptr env_pick_2(const char* name){
  ZsArgs args{1};
  auto num = args[0].get<Number*>();
  if(!num){
    throw builtin_type_check_failed(name, Ptr_tag::number, args[0]);
  }

  if(num->type() != Number::Type::integer){
    throw zs_error("native func: %s: passed number is not exact integer\n", name);
  }

  auto ver = num->get<Number::integer_type>();
  if(ver != 5l){
    throw zs_error("native func: %s: passed number is not 5 (supplied %ld)\n",
                        name, ver);
  }

  return vm.frame()->find(intern(vm.symtable(), name));
}

Lisp_ptr env_r5rs(){
  return env_pick_2(r5rs_env_symname);
}

Lisp_ptr env_null(){
  return env_pick_2(null_env_symname);
}

Lisp_ptr env_interactive(){
  ZsArgs args{0};
  return vm.frame()->find(intern(vm.symtable(), interaction_env_symname));
}
  

Lisp_ptr eval_func(){
  ZsArgs args{2};
  auto env = args[1].get<Env*>();
  if(!env){
    throw builtin_type_check_failed("eval", Ptr_tag::env, args[1]);
  }

  auto oldenv = vm.frame();
  vm.set_frame(env);
  vm.return_value = {oldenv, vm_op_leave_frame, args[0]};
  return {};
}


void load_internal(const string& str){
  istringstream ss{str};
  while(ss){
    auto form = read(ss);
    if(!form){
      if(!ss){
        // cerr << "load error: failed at reading a form. abandoned.\n";
      }
      break;
    }

    vm.code.push_back(form);
    eval();
    if(!vm.return_value_1()){
      cerr << "load error: failed at evaluating a form. skipped.\n";
      cerr << "\tform: \n";
      print(cerr, form);
      continue;
    }
  }
}

Lisp_ptr load_func(){
  ZsArgs args{1};
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed("load", Ptr_tag::string, args[0]);
  }

  load_internal(*str);
  return Lisp_ptr{true};
}

} //namespace

static const BuiltinNProc builtin_syntax_funcs[] = {
#include "builtin_syntax.defs.hh"
};

static const BuiltinNProc builtin_funcs[] = {
  {"eval", {
      eval_func,
      {2, 2, Passing::eval, Returning::code, MoveReturnValue::f}}},

  {"scheme-report-environment", {
      env_r5rs,
      {1}}},
  {"null-environment", {
      env_null,
      {1}}},
  {"interaction-environment", {
      env_interactive,
      {0}}},

  {"load", {
      load_func,
      {1}}},

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
#include "builtin_cons.strs.hh"
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
    load_internal(s);
  };    
  static constexpr auto install_builtin_symbol = [](const char* name, Lisp_ptr value){
    vm.frame()->local_set(intern(vm.symtable(), name), value);
  };    

  // null-environment
  for_each(std::begin(builtin_syntax_funcs), std::end(builtin_syntax_funcs),
           install_builtin_native);
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

  return nullptr;
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
