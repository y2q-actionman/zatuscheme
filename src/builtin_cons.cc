#include "builtin_cons.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace zs {
namespace builtin {

Lisp_ptr cons_pairp(ZsArgs args){
  return Lisp_ptr{is_nonnull_cons(args[0])};
}

Lisp_ptr cons_cons(ZsArgs args){
  return {zs_new<Cons>(args[0], args[1])};
}

Lisp_ptr cons_car(ZsArgs args){
  check_nonnull_cons(args[0]);
  return args[0].get<Cons*>()->car;
}

Lisp_ptr cons_cdr(ZsArgs args){
  check_nonnull_cons(args[0]);
  return args[0].get<Cons*>()->cdr;
}

Lisp_ptr cons_set_car(ZsArgs args){
  check_nonnull_cons(args[0]);
  return (args[0].get<Cons*>()->car = args[1]);
}

Lisp_ptr cons_set_cdr(ZsArgs args){
  check_nonnull_cons(args[0]);
  return (args[0].get<Cons*>()->cdr = args[1]);
}

} // namespace builtin
} // namespace zs
