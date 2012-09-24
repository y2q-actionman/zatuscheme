#include <array>

#include "builtin.hh"
#include "util.hh"
#include "number.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "printer.hh"

using namespace std;
using namespace Procedure;

namespace {

template<Number::Type nt>
void number_type_check(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  VM.return_value = Lisp_ptr{num->type() == nt};
}

void number_type_check_rational(){
  pick_args_1();
  VM.return_value = Lisp_ptr{false};
}  

void plus_2(){
  auto args = pick_args<2>();

  VM.return_value = {};

  Number* n1 = args[0].get<Number*>();
  if(!n1){
    fprintf(zs::err, "native func '+': first arg is not number! %s\n",
            stringify(args[0].tag()));
    return;
  }

  Number* n2 = args[1].get<Number*>();
  if(!n2){
    fprintf(zs::err, "native func '+': second arg is not number! %s\n",
            stringify(args[1].tag()));
    return;
  }

  Number* newn = new Number(n1->get<long>() + n2->get<long>());
  VM.return_value = newn;
}


constexpr struct Entry {
  const char* name;
  const NProcedure func;

  constexpr Entry(const char* n, const NProcedure& f)
    : name(n), func(f){}
} builtin_numeric[] = {
  {"complex?", {
      number_type_check<Number::Type::complex>,
      Calling::function, {1, false}}},
  {"real?", {
      number_type_check<Number::Type::real>,
      Calling::function, {1, false}}},
  {"integer?", {
      number_type_check<Number::Type::integer>,
      Calling::function, {1, false}}},
  {"rational?", {
      number_type_check_rational,
      Calling::function, {1, false}}},

  {"+", {
      plus_2,
      Calling::function, {2, true}}}
};

} //namespace

void install_builtin_numeric(){
  for(auto& e : builtin_numeric){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
