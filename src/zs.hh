#ifndef ZS_HH
#define ZS_HH

#include "builtin.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "decl.hh"
#include "env.hh"
#include "equality.hh"
#include "eval.hh"
#include "lisp_ptr.hh"
#include "printer.hh"
#include "procedure.hh"
#include "rational.hh"
#include "reader.hh"
#include "s_closure.hh"
#include "s_rules.hh"
#include "symbol.hh"
#include "token.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

void zs_init();
int zs_exit_status();

#endif //ZS_HH
