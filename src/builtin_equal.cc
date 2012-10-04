#include <array>

#include "builtin_equal.hh"
#include "util.hh"
#include "lisp_ptr.hh"
#include "number.hh"
#include "procedure.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "vm.hh"

using namespace std;
using namespace Procedure;

namespace {


bool eq_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;

  if(a.tag() == Ptr_tag::boolean){
    return a.get<bool>() == b.get<bool>();
  }else if(a.tag() == Ptr_tag::character){
     // this can be moved into eqv? in R5RS, but char is contained in Lisp_ptr.
    return a.get<char>() == b.get<char>();
  }else{
    return a.get<void*>() == b.get<void*>();
  }
}

bool eqv_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() == Ptr_tag::number && b.tag() == Ptr_tag::number){
    return eqv(*a.get<Number*>(), *b.get<Number*>());
  }else{
    return eq_internal(a, b);
  }
}

void eq(){
  auto args = pick_args<2>();
  VM.return_value = Lisp_ptr{eq_internal(args[0], args[1])};
}

void eqv(){
  auto args = pick_args<2>();
  VM.return_value = Lisp_ptr{eqv_internal(args[0], args[1])};
}

} //namespace

const BuiltinFunc
builtin_equal[] = {
  {"eqv?", {
      eqv,
      {Calling::function, 2}}},
  {"eq?", {
      eq,
      {Calling::function, 2}}},
};

const size_t builtin_equal_size = sizeof(builtin_equal) / sizeof(builtin_equal[0]);
