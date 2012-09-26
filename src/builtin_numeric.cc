#include <array>
#include <iterator>
#include <vector>

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

void complexp(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  auto t = num->type();
  VM.return_value = Lisp_ptr{t == Number::Type::complex
                             || t == Number::Type::real
                             || t == Number::Type::integer};
}

void realp(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  auto t = num->type();
  VM.return_value = Lisp_ptr{t == Number::Type::real
                             || t == Number::Type::integer};
}

void rationalp(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  auto t = num->type();
  VM.return_value = Lisp_ptr{t == Number::Type::integer};
}

void integerp(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  auto t = num->type();
  VM.return_value = Lisp_ptr{t == Number::Type::integer};
}

void exactp(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  auto t = num->type();
  VM.return_value = Lisp_ptr{t == Number::Type::integer};
}

void inexactp(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  auto t = num->type();
  VM.return_value = Lisp_ptr{t == Number::Type::complex
                             || t == Number::Type::real};
}

void number_equal(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  auto n1 = args.front().get<Number*>();
  if(!n1){
    fprintf(zs::err, "native func '=': arg is not number! (%s)\n",
            stringify(args.front().tag()));
    VM.return_value = {};
    return;
  }

  const auto n_type = n1->type();

  for(auto i = next(begin(args)), e = end(args);
      i != e; ++i){
    auto n2 = i->get<Number*>();
    if(!n2){
      fprintf(zs::err, "native func '=': arg is not number! (%s)\n",
              stringify(i->tag()));
      VM.return_value = {};
      return;
    }

    if(n2->type() != n_type){
      VM.return_value = Lisp_ptr{false};
      return;
    }

    switch(n_type){
    case Number::Type::complex:
      if(n1->get<Number::complex_type>() != n2->get<Number::complex_type>()){
        VM.return_value = Lisp_ptr{false};
        return;
      }
      break;
    case Number::Type::real:
      if(n1->get<Number::real_type>() != n2->get<Number::real_type>()){
        VM.return_value = Lisp_ptr{false};
        return;
      }
      break;
    case Number::Type::integer:
      if(n1->get<Number::integer_type>() != n2->get<Number::integer_type>()){
        VM.return_value = Lisp_ptr{false};
        return;
      }
      break;
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }
  }

  VM.return_value = Lisp_ptr{true};
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
      complexp,
      Calling::function, {1, false}}},
  {"real?", {
      realp,
      Calling::function, {1, false}}},
  {"rational?", {
      rationalp,
      Calling::function, {1, false}}},
  {"integer?", {
      integerp,
      Calling::function, {1, false}}},

  {"exact?", {
      exactp,
      Calling::function, {1, false}}},
  {"inexact?", {
      inexactp,
      Calling::function, {1, false}}},

  {"=", {
      number_equal,
      Calling::function, {2, true}}},

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
