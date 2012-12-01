#include <array>
#include <algorithm>

#include "builtin_equal.hh"
#include "util.hh"
#include "lisp_ptr.hh"
#include "number.hh"
#include "procedure.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "vm.hh"
#include "cons.hh"

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

bool equal_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() == Ptr_tag::cons && b.tag() == Ptr_tag::cons){
    return do_list_2(a, b,
                     [](Cons* c1, Cons* c2){
                       return equal_internal(c1->car(), c2->car());
                     },
                     [](Lisp_ptr a_last, Lisp_ptr b_last){
                       return nullp(a_last) && nullp(b_last);
                     });
  }else if(a.tag() == Ptr_tag::vector && b.tag() == Ptr_tag::vector){
    auto v1 = a.get<Vector*>(), v2 = b.get<Vector*>();
    return std::equal(v1->begin(), v1->end(), v2->begin(), equal_internal);
  }else if(a.tag() == Ptr_tag::string && b.tag() == Ptr_tag::string){
    auto s1 = a.get<String*>(), s2 = b.get<String*>();
    return *s1 == *s2;
  }else{
    return eqv_internal(a, b);
  }
}

Lisp_ptr eq(){
  ZsArgs args{2};
  return Lisp_ptr{eq_internal(args[0], args[1])};
}

Lisp_ptr eqv(){
  ZsArgs args{2};
  return Lisp_ptr{eqv_internal(args[0], args[1])};
}

Lisp_ptr equal(){
  ZsArgs args{2};
  return Lisp_ptr{equal_internal(args[0], args[1])};
}

} //namespace

const BuiltinFunc
builtin_equal[] = {
  {"eq?", {
      eq,
      {Calling::function, 2}}},
  {"eqv?", {
      eqv,
      {Calling::function, 2}}},
  {"equal?", {
      equal,
      {Calling::function, 2}}}
};

const size_t builtin_equal_size = sizeof(builtin_equal) / sizeof(builtin_equal[0]);
