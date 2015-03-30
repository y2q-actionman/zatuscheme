#include "builtin_equal.hh"
#include "equality.hh"
#include "lisp_ptr.hh"
#include "vm.hh"

using namespace std;

namespace zs {
namespace builtin {

Lisp_ptr eq(ZsArgs args){
  return Lisp_ptr{zs::eq(args[0], args[1])};
}

Lisp_ptr eqv(ZsArgs args){
  return Lisp_ptr{zs::eqv(args[0], args[1])};
}

Lisp_ptr equal(ZsArgs args){
  return Lisp_ptr{zs::equal(args[0], args[1])};
}

} // namespace builtin
} // namespace zs
