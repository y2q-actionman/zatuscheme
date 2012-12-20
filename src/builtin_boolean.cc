#include "builtin_boolean.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"

using namespace std;

Lisp_ptr not_func(){
  ZsArgs args{1};
  return Lisp_ptr{!args[0].get<bool>()};
}
