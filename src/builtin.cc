#include <array>

#include "builtin.hh"
#include "util.hh"
#include "number.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "vm.hh"
#include "port.hh"

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
using namespace Procedure;

namespace {

static const char null_env_symname[] = "null-env-value";
static const char r5rs_env_symname[] = "r5rs-env-value";
static const char interaction_env_symname[] = "interaction-env-value";

static
void env_pick_2(const char* name){
  auto p = pick_args_1();
  auto num = p.get<Number*>();
  if(!num){
    builtin_type_check_failed(name, Ptr_tag::number, p);
    return;
  }

  if(num->type() != Number::Type::integer){
    fprintf(zs::err, "native func: %s: passed number is not exact integer\n", name);
    VM.return_value = {};
    return;
  }

  auto ver = num->get<Number::integer_type>();
  if(ver != 5l){
    fprintf(zs::err, "native func: %s: passed number is not 5 (supplied %ld)\n",
            name, ver);
    VM.return_value = {};
    return;
  }

  VM.return_value = VM.find(intern(VM.symtable, name));
}

void env_r5rs(){
  env_pick_2(r5rs_env_symname);
}

void env_null(){
  env_pick_2(null_env_symname);
}

void env_interactive(){
  pick_args<0>();
  VM.return_value = VM.find(intern(VM.symtable, interaction_env_symname));
}
  

void eval_func(){
  auto args = pick_args<2>();
  auto env = args[1].get<Env*>();
  if(!env){
    builtin_type_check_failed("eval", Ptr_tag::env, args[1]);
    return;
  }

  VM.enter_frame(env->push());
  VM.code.push(vm_op_leave_frame);
  VM.code.push(args[0]);
}


void load_func(){
  auto arg = pick_args_1();
  auto str = arg.get<String*>();
  if(!str){
    builtin_type_check_failed("load", Ptr_tag::string, arg);
    return;
  }

  Port p{str->c_str(), "r"};
  if(!p){
    fprintf(zs::err, "load error: failed at opening file\n");
    VM.return_value = {};
    return;
  }

  load(&p);
  VM.return_value = {};
}

} //namespace

static const BuiltinFunc
builtin_misc[] = {
  {"eval", {
      eval_func,
      {Calling::function, 2}}},

  {"scheme-report-environment", {
      env_r5rs,
      {Calling::function, 1}}},
  {"null-environment", {
      env_null,
      {Calling::function, 1}}},
  {"interaction-environment", {
      env_interactive,
      {Calling::function, 0}}},

  {"load", {
      load_func,
      {Calling::function, 1}}}
};


static void install_builtin_internal(const BuiltinFunc bf[], size_t s){
  for(size_t i = 0; i < s; ++i){
    VM.local_set(intern(VM.symtable, bf[i].name), {&bf[i].func});
  }
}

void install_builtin(){
  install_builtin_internal(builtin_equal, builtin_equal_size);
  install_builtin_internal(builtin_syntax, builtin_syntax_size);
  VM.local_set(intern(VM.symtable, null_env_symname), VM.frame);

  VM.frame = VM.frame->push();
  install_builtin_internal(builtin_misc, sizeof(builtin_misc) / sizeof(builtin_misc[0]));
  install_builtin_internal(builtin_boolean, builtin_boolean_size);
  install_builtin_internal(builtin_char, builtin_char_size);
  install_builtin_internal(builtin_cons, builtin_cons_size);
  install_builtin_internal(builtin_numeric, builtin_numeric_size);
  install_builtin_internal(builtin_procedure, builtin_procedure_size);
  install_builtin_internal(builtin_string, builtin_string_size);
  install_builtin_internal(builtin_symbol, builtin_symbol_size);
  install_builtin_internal(builtin_vector, builtin_vector_size);
  install_builtin_port_value();
  install_builtin_internal(builtin_port, builtin_port_size);
  VM.local_set(intern(VM.symtable, r5rs_env_symname), VM.frame);

  VM.frame = VM.frame->push();
  install_builtin_internal(builtin_extra, builtin_extra_size);
  VM.local_set(intern(VM.symtable, interaction_env_symname), VM.frame);
}
