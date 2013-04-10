#include "builtin_equal.hh"
#include "lisp_ptr.hh"
#include "builtin_util.hh"
#include "vm.hh"

using namespace std;

namespace builtin {

Lisp_ptr eq(ZsArgs args){
  return Lisp_ptr{eq_internal(args[0], args[1])};
}

Lisp_ptr eqv(ZsArgs args){
  return Lisp_ptr{eqv_internal(args[0], args[1])};
}

Lisp_ptr equal(ZsArgs args){
  return Lisp_ptr{equal_internal(args[0], args[1])};
}

} // namespace builtin
