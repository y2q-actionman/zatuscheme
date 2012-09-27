#include <array>
#include <iterator>
#include <vector>
#include <functional>
#include <algorithm>
#include <numeric>

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

template<typename Fun>
inline void number_pred(Fun&& fun){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  VM.return_value = Lisp_ptr{fun(num)};
}

void complexp(){
  number_pred([](Number* n){
      return n->type() >= Number::Type::integer;
    });
}

void realp(){
  number_pred([](Number* n){
      return n->type() >= Number::Type::integer
        && n->type() <= Number::Type::real;
    });
}

void rationalp(){
  number_pred([](Number* n){
      return n->type() >= Number::Type::integer
        && n->type() < Number::Type::real;
    });
}

void integerp(){
  number_pred([](Number* n){
      return n->type() == Number::Type::integer;
    });
}

void exactp(){
  number_pred([](Number* n){
      return n->type() == Number::Type::integer;
    });
}

void inexactp(){
  number_pred([](Number* n){
      return n->type() == Number::Type::complex
        || n->type() == Number::Type::real;
    });
}


void number_type_check_failed(const char* func_name, Lisp_ptr p){
  fprintf(zs::err, "native func: %s: arg is not number! (%s)\n",
          func_name, stringify(p.tag()));
  VM.return_value = {};
}

void number_equal(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  Lisp_ptr non_number_found = {};

  auto ret = std::is_sorted(begin(args), end(args),
                            [&](const Lisp_ptr& p1, const Lisp_ptr& p2) -> bool {
                              auto n1 = p1.get<Number*>();
                              if(!n1 || n1->type() < Number::Type::integer){
                                non_number_found = p1;
                                return true;
                              }
                              
                              auto n2 = p2.get<Number*>();
                              if(!n2 || n2->type() < Number::Type::integer){
                                non_number_found = p2;
                                return true;
                              }

                              if(n1->type() == Number::Type::integer && n2->type() == Number::Type::integer){
                                return (n1->get<Number::integer_type>()
                                        == n2->get<Number::integer_type>()) ? false : true;
                              }else if(n1->type() <= Number::Type::real && n2->type() <= Number::Type::real){
                                return (n1->coerce<Number::real_type>()
                                        == n2->coerce<Number::real_type>()) ? false : true;
                              }else{
                                return (n1->coerce<Number::complex_type>()
                                        == n2->coerce<Number::complex_type>()) ? false : true;
                              }
                            });

  if(non_number_found){
    number_type_check_failed("=", non_number_found);
    return;
  }

  VM.return_value = Lisp_ptr{ret};
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
  number_pred([](Number* num) -> bool {
      switch(num->type()){
      case Number::Type::complex: {
        auto c = num->get<Number::complex_type>();
        return (c.real() == 0 && c.imag() == 0);
      }
      case Number::Type::real:
        return num->get<Number::real_type>() == 0;
      case Number::Type::integer:
        return num->get<Number::integer_type>() == 0;
      case Number::Type::uninitialized:
      default:
        UNEXP_DEFAULT();
      }
    });
}

template<template <typename> class Fun>
struct pos_neg_pred{
  inline bool operator()(Number* num){
    static constexpr Fun<Number::integer_type> fun;

    switch(num->type()){
    case Number::Type::complex:
      return false;
    case Number::Type::real:
      return fun(num->get<Number::real_type>(), 0);
    case Number::Type::integer:
      return fun(num->get<Number::integer_type>(), 0);
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }
  }
};

void positivep(){
  number_pred(pos_neg_pred<std::greater>());
}

void negativep(){
  number_pred(pos_neg_pred<std::less>());
}

template<template <typename> class Fun>
struct even_odd_pred{
  inline bool operator()(Number* num){
    static constexpr Fun<Number::integer_type> fun;

    switch(num->type()){
    case Number::Type::complex:
    case Number::Type::real:
      return false;
    case Number::Type::integer:
      return fun(num->get<Number::integer_type>() % 2, 0);
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }
  }
};

void oddp(){
  number_pred(even_odd_pred<std::not_equal_to>());
}

void evenp(){
  number_pred(even_odd_pred<std::equal_to>());
}


template<template <typename> class Fun>
void number_minmax(const char* name){
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

  auto minmax_n_obj = n_obj;
  auto minmax_n = n_obj->coerce<Number::real_type>();  

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

    if(fun(n2, minmax_n)){
      minmax_n_obj = n_obj;
      minmax_n = n2;
    }
  }

  VM.return_value = Lisp_ptr{minmax_n_obj};
}

void number_max(){
  number_minmax<std::greater>("max");
}

void number_min(){
  number_minmax<std::less>("min");
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

  {"max", {
      number_max,
      Calling::function, {2, true}}},
  {"min", {
      number_min,
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
