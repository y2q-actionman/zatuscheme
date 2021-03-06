#include <iostream>
#include <istream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include "builtin.hh"
#include "eval.hh"
#include "env.hh"
#include "lisp_ptr.hh"
#include "procedure.hh"
#include "reader.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"
#include "zs_scm_include.hh"

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
using namespace zs::proc_flag;

namespace zs {

void load_from_stream(std::istream& ss){
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
  check_type(Ptr_tag::env, args[1]);

  auto env = args[1].get<Env*>();

  auto oldenv = vm.frame;
  vm.frame = env;
  vm.code.insert(vm.code.end(),
                 {oldenv, vm_op_leave_frame, args[0]});
  return {};
}

Lisp_ptr load(ZsArgs args){
  check_type(Ptr_tag::string, args[0]);

  ifstream ifs{*args[0].get<String*>()};
  load_from_stream(ifs);
  return Lisp_ptr{true};
}

Lisp_ptr push_environment(ZsArgs){
  vm.frame = vm.frame->push();
  return Lisp_ptr{vm.frame};
}

} // namespace builtin

static const NProcedure builtin_funcs[] = {
#include "builtin.defs.hh"
#include "builtin_boolean.defs.hh"
#include "builtin_char.defs.hh"
#include "builtin_cons.defs.hh"
#include "builtin_equal.defs.hh"
#include "builtin_extra.defs.hh"
#include "builtin_numeric.defs.hh"
#include "builtin_port.defs.hh"
#include "builtin_procedure.defs.hh"
#include "builtin_string.defs.hh"
#include "builtin_symbol.defs.hh"
#include "builtin_syntax.defs.hh"
#include "builtin_vector.defs.hh"
};

static const char* builtin_str =
#include "builtin_syntax.scm"

// sequenced by dependencies
#include "builtin_r5rs_begin.scm"
#include "builtin_symbol.scm"	// (independent)
#include "builtin_equal.scm"	// (independent)
#include "builtin_boolean.scm"	// equal
#include "builtin_cons.scm"	// equal
#include "builtin_procedure.scm" // cons
#include "builtin_numeric.scm"	// boolean, cons, procedure
#include "builtin_char.scm"	// numeric
#include "builtin_vector.scm"	// cons, numeric, procedure
#include "builtin_string.scm"	// cons, char, numeric, procedure
#include "builtin_port.scm"	// cons, procedure
#include "builtin.scm"		// numeric
#include "builtin_r5rs_end.scm"

#include "builtin_extra.scm"
;

static void install_symbol(const char* name, Lisp_ptr value){
  vm.frame->local_set(intern(*vm.symtable, name), value);
}

void install_builtin(){
  assert(vm.code.empty() && vm.stack.empty());
  assert(!vm.frame);

  // symtable
  assert(!vm.symtable);
  vm.symtable.reset(new SymTable());

  // null-environment
  vm.frame = zs_new<Env>(nullptr);

  // internal symbols (prefixed %)
  install_symbol(EXPAND_STRINGIFY(CURRENT_INPUT_PORT_SYMNAME), &std::cin);
  install_symbol(EXPAND_STRINGIFY(CURRENT_OUTPUT_PORT_SYMNAME), &std::cout);

  for(auto& i : builtin_funcs)
    vm.frame->local_set(intern(*vm.symtable, i.name()), {&i});

  // interpreted start!
  istringstream iss{builtin_str};
  load_from_stream(iss);
  start_evaluation();
}

} // namespace zs
