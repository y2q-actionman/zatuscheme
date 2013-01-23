#include "builtin_equal.hh"
#include "lisp_ptr.hh"
#include "builtin_util.hh"
#include "vm.hh"

using namespace std;

Lisp_ptr eq_proc(){
  ZsArgs args{2};
  return Lisp_ptr{eq_internal(args[0], args[1])};
}

Lisp_ptr eqv_proc(){
  ZsArgs args{2};
  return Lisp_ptr{eqv_internal(args[0], args[1])};
}

Lisp_ptr equal_proc(){
  ZsArgs args{2};
  return Lisp_ptr{equal_internal(args[0], args[1])};
}
