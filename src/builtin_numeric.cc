#include <array>
#include <iterator>
#include <vector>
#include <functional>

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


void number_type_check_failed(const char* func_name, Lisp_ptr p){
  fprintf(zs::err, "native func: %s: arg is not number! (%s)\n",
          func_name, stringify(p.tag()));
  VM.return_value = {};
}

void number_equal(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  auto n1 = args.front().get<Number*>();
  if(!n1){
    number_type_check_failed("=", args.front());
    return;
  }

  const auto n_type = n1->type();

  for(auto i = next(begin(args)), e = end(args);
      i != e; ++i){
    auto n2 = i->get<Number*>();
    if(!n2){
      number_type_check_failed("=", *i);
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


bool number_comparable_check(Number* n){
  switch(n->type()){
  case Number::Type::complex:
    fprintf(zs::err, "native func: number compare: complex cannot be ordinated\n");
    VM.return_value = {};
    return false;
  case Number::Type::real:
  case Number::Type::integer:
    return true;
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}

template<template <typename> class Fun>
void number_compare(const char* name){
  static constexpr Fun<Number::real_type> fun;

  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  auto n_obj = args.front().get<Number*>();
  if(!n_obj){
    number_type_check_failed(name, args.front());
    return;
  }

  if(!number_comparable_check(n_obj)){
    return;
  }

  auto n1 = n_obj->coerce<Number::real_type>();  

  for(auto i = next(begin(args)), e = end(args);
      i != e; ++i){
    n_obj = i->get<Number*>();
    if(!n_obj){
      number_type_check_failed(name, *i);
      return;
    }

    if(!number_comparable_check(n_obj)){
      return;
    }

    auto n2 = n_obj->coerce<Number::real_type>();

    if(!fun(n1, n2)){
      VM.return_value = Lisp_ptr{false};
      return;
    }

    n1 = n2;
  }

  VM.return_value = Lisp_ptr{true};
}

void number_less(){
  number_compare<std::less>("<");
}
  
void number_greater(){
  number_compare<std::greater>(">");
}
  
void number_less_eq(){
  number_compare<std::less_equal>("<=");
}
  
void number_greater_eq(){
  number_compare<std::greater_equal>(">=");
}
  

void zerop(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  switch(num->type()){
  case Number::Type::complex: {
    auto c = num->get<Number::complex_type>();
    VM.return_value = Lisp_ptr{c.real() == 0 && c.imag() == 0};
    return;
  }
  case Number::Type::real:
    VM.return_value = Lisp_ptr{num->get<Number::real_type>() == 0};
    return;
  case Number::Type::integer:
    VM.return_value = Lisp_ptr{num->get<Number::integer_type>() == 0};
    return;
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}


template<template <typename> class Fun>
void number_positive_negative(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  switch(num->type()){
  case Number::Type::complex:
    VM.return_value = Lisp_ptr{false};
    return;
  case Number::Type::real: {
    auto n = num->get<Number::real_type>();
    auto fun = Fun<decltype(n)>();
    VM.return_value = Lisp_ptr{fun(n, 0)};
    return;
  }
  case Number::Type::integer: {
    auto n = num->get<Number::integer_type>();
    auto fun = Fun<decltype(n)>();
    VM.return_value = Lisp_ptr{fun(n, 0)};
    return;
  }
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}

void positivep(){
  number_positive_negative<std::greater>();
}

void negativep(){
  number_positive_negative<std::less>();
}


template<template <typename> class Fun>
void number_odd_even(){
  static constexpr Fun<Number::integer_type> fun;

  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  switch(num->type()){
  case Number::Type::complex:
  case Number::Type::real:
    VM.return_value = Lisp_ptr{false};
    return;
  case Number::Type::integer:
    VM.return_value = Lisp_ptr{fun(num->get<Number::integer_type>() % 2, 0)};
    return;
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}

void oddp(){
  number_odd_even<std::not_equal_to>();
}

void evenp(){
  number_odd_even<std::equal_to>();
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
  {"<", {
      number_less,
      Calling::function, {2, true}}},
  {">", {
      number_greater,
      Calling::function, {2, true}}},
  {"<=", {
      number_less_eq,
      Calling::function, {2, true}}},
  {">=", {
      number_greater_eq,
      Calling::function, {2, true}}},

  {"zero?", {
      zerop,
      Calling::function, {1, false}}},
  {"positive?", {
      positivep,
      Calling::function, {1, false}}},
  {"negative?", {
      negativep,
      Calling::function, {1, false}}},
  {"odd?", {
      oddp,
      Calling::function, {1, false}}},
  {"even?", {
      evenp,
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
