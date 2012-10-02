#include <array>
#include <iterator>
#include <vector>
#include <functional>
#include <algorithm>
#include <numeric>
#include <cstdlib>
#include <cmath>
#include <limits>

#include "builtin_numeric.hh"
#include "vm.hh"
#include "builtin.hh"
#include "util.hh"
#include "number.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "port.hh"

using namespace std;
using namespace Procedure;

namespace {

void number_type_check_failed(const char* func_name, Lisp_ptr p){
  builtin_type_check_failed(func_name, Ptr_tag::number, p);
}

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

struct complex_found{
  static constexpr const char* msg = "native func: number compare: complex cannot be ordinated\n";
  bool operator()(const Number::complex_type&, const Number::complex_type&) const{
    fprintf(zs::err, msg);
    return false;
  }
};

template<template <typename> class Fun,
         class ComplexComparator = complex_found>
struct number_comparator {
  inline bool operator()(const Number* n1, const Number* n2){
    if(n1->type() == Number::Type::integer && n2->type() == Number::Type::integer){
      static const Fun<Number::integer_type> fun;
      return fun(n1->get<Number::integer_type>(), n2->get<Number::integer_type>());
    }else if(n1->type() <= Number::Type::real && n2->type() <= Number::Type::real){
      static const Fun<Number::real_type> fun;
      return fun(n1->coerce<Number::real_type>(), n2->coerce<Number::real_type>());
    }else{
      static const ComplexComparator fun;
      return fun(n1->coerce<Number::complex_type>(), n2->coerce<Number::complex_type>());
    }
  }
};


template<typename Fun>
inline void number_compare(const char* name, Fun&& fun){
  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  auto i1 = begin(args);
  const auto e = end(args);

  auto n1 = i1->get<Number*>();
  if(!n1 || n1->type() < Number::Type::integer){
    number_type_check_failed(name, *i1);
    return;
  }
                              
  for(auto i2 = next(i1); i2 != e; i1 = i2, ++i2){
    auto n2 = i2->get<Number*>();
    if(!n2 || n2->type() < Number::Type::integer){
      number_type_check_failed(name, *i2);
      return;
    }

    if(!fun(n1, n2)){
      VM.return_value = Lisp_ptr{false};
      return;
    }

    n1 = n2;
  }

  VM.return_value = Lisp_ptr{true};
}

void number_equal(){
  number_compare("=", 
                 number_comparator<std::equal_to,
                                   std::equal_to<Number::complex_type> >());
}

void number_less(){
  number_compare("<",
                 number_comparator<std::less>()); 
}

void number_greater(){
  number_compare(">",
                 number_comparator<std::greater>());
}
  
void number_less_eq(){
  number_compare("<=",
                 number_comparator<std::less_equal>());
}
  
void number_greater_eq(){
  number_compare(">=",
                 number_comparator<std::greater_equal>());
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


template<class Fun>
inline
void number_accumulate(const char* name, Number&& init, Fun&& fun){
  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  for(auto i = begin(args), e = end(args);
      i != e; ++i){
    auto n = i->get<Number*>();
    if(!n){
      number_type_check_failed(name, *i);
      return;
    }

    if(!fun(init, *n)){
      VM.return_value = {};
      return;
    }
  }

  VM.return_value = {new Number(init)};
}


template<template <typename> class Cmp>
struct minmax_accum{
  inline bool operator()(Number& n1, const Number& n2){
    if(n1.type() == Number::Type::uninitialized){
      n1 = n2;
      return true;
    }

    if(n2.type() == Number::Type::uninitialized){
      return true;
    }

    if(n1.type() == Number::Type::integer && n2.type() == Number::Type::integer){
      static constexpr Cmp<Number::integer_type> cmp;

      if(cmp(n2.get<Number::integer_type>(), n1.get<Number::integer_type>()))
        n1 = n2;
      return true;
    }

    if(n1.type() <= Number::Type::real && n2.type() <= Number::Type::real){
      static constexpr Cmp<Number::real_type> cmp;

      if(cmp(n2.coerce<Number::real_type>(), n1.coerce<Number::real_type>())){
        n1 = Number{n2.coerce<Number::real_type>()};
      }else if(n1.type() != Number::Type::real){
        n1 = Number{n1.coerce<Number::real_type>()};
      }
      return true;
    }

    fprintf(zs::err, complex_found::msg);
    return false;
  }
};    


void number_max(){
  number_accumulate("max", Number(), minmax_accum<std::greater>());
}

void number_min(){
  number_accumulate("min", Number(), minmax_accum<std::less>());
}

template<template <typename> class Op>
struct binary_accum{
  inline bool operator()(Number& n1, const Number& n2){
    if(n1.type() == Number::Type::uninitialized){
      n1 = n2;
      return true;
    }

    if(n2.type() == Number::Type::uninitialized){
      return true;
    }

    // n1 type <= n2 type
    if(n1.type() == Number::Type::integer && n2.type() == Number::Type::integer){
      // static constexpr auto imax = numeric_limits<Number::integer_type>::max();
      // static constexpr auto imin = numeric_limits<Number::integer_type>::min();
      static constexpr Op<Number::integer_type> op;

      long long tmp = op(n1.get<Number::integer_type>(), n2.get<Number::integer_type>());
      // if(tmp > imax || tmp < imin){
      //   fprintf(zs::err, "integer operation fallen into float\n");
      //   n1 = Number{static_cast<Number::real_type>(tmp)};
      // }else{
        n1.get<Number::integer_type>() = static_cast<Number::integer_type>(tmp);
      // }

      return true;
    }

    if(n1.type() == Number::Type::real && n2.type() <= Number::Type::real){
      static constexpr Op<Number::real_type> op;
      n1.get<Number::real_type>() = 
        op(n1.get<Number::real_type>(), n2.coerce<Number::real_type>());
      return true;
    }

    if(n1.type() == Number::Type::complex && n2.type() <= Number::Type::complex){
      static constexpr Op<Number::complex_type> op;
      n1.get<Number::complex_type>() = 
        op(n1.get<Number::complex_type>(), n2.coerce<Number::complex_type>());
      return true;
    }

    // n1 type > n2 type
    if(n1.type() < Number::Type::real && n2.type() == Number::Type::real){
      static constexpr Op<Number::real_type> op;
      n1 = Number{op(n1.coerce<Number::real_type>(), n2.get<Number::real_type>())};
      return true;
    }

    if(n1.type() < Number::Type::complex && n2.type() == Number::Type::complex){
      static constexpr Op<Number::complex_type> op;
      n1 = Number{op(n1.coerce<Number::complex_type>(), n2.get<Number::complex_type>())};
      return true;
    }

    // ???
    fprintf(zs::err, "native func: +-*/: failed at numeric conversion!\n");
    return false;
  }
};


void number_plus(){
  number_accumulate("+", Number(0l), binary_accum<std::plus>());
}

void number_multiple(){
  number_accumulate("*", Number(1l), binary_accum<std::multiplies>());
}

void number_minus(){
  auto arg1 = VM.stack.top();
  VM.stack.pop();

  auto n = arg1.get<Number*>();
  if(!n){
    number_type_check_failed("-", arg1);
    clean_args();
    return;
  }

  if(VM.stack.top().tag() == Ptr_tag::vm_op){
    VM.stack.pop();

    switch(n->type()){
    case Number::Type::integer: {
      static constexpr auto imin = numeric_limits<Number::integer_type>::min();
      auto i = n->get<Number::integer_type>();
      if(i == imin){
        fprintf(zs::err, "integer operation fallen into float\n");
        VM.return_value = {new Number(-static_cast<Number::real_type>(imin))};
      }else{
        VM.return_value = {new Number(-i)};
      }
      return;
    }
    case Number::Type::real:
      VM.return_value = {new Number(-n->get<Number::real_type>())};
      return;
    case Number::Type::complex: {
      auto c = n->get<Number::complex_type>();
      VM.return_value = {new Number(Number::complex_type(-c.real(), -c.imag()))};
      return;
    }
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }
  }
   
  number_accumulate("-", Number(*n), binary_accum<std::minus>());
}

void number_divide(){
  auto arg1 = VM.stack.top();
  VM.stack.pop();

  auto n = arg1.get<Number*>();
  if(!n){
    number_type_check_failed("/", arg1);
    clean_args();
    return;
  }

  if(VM.stack.top().tag() == Ptr_tag::vm_op){
    VM.stack.pop();

    switch(n->type()){
    case Number::Type::integer:
      VM.return_value = {new Number(1.0 / n->get<Number::integer_type>())};
      return;
    case Number::Type::real:
      VM.return_value = {new Number(1.0 / n->get<Number::real_type>())};
      return;
    case Number::Type::complex: {
      auto c = n->get<Number::complex_type>();
      VM.return_value = {new Number(1.0 / c)};
      return;
    }
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }
  }

  number_accumulate("/", 
                    n->type() == Number::Type::integer
                    ? Number(n->coerce<Number::real_type>()) : Number(*n),                    
                    binary_accum<std::divides>());
}

void number_abs(){
  auto arg1 = pick_args_1();

  auto n = arg1.get<Number*>();
  if(!n){
    number_type_check_failed("abs", arg1);
    return;
  }

  switch(n->type()){
  case Number::Type::integer: {
    auto i = n->get<Number::integer_type>();
    if(i >= 0){
      VM.return_value = n;
    }else{
      static constexpr auto imin = numeric_limits<Number::integer_type>::min();
      if(i == imin){
        fprintf(zs::err, "integer operation fallen into float\n");
        VM.return_value = new Number(-static_cast<Number::real_type>(imin));
      }else{
        VM.return_value = new Number(-i);
      }
    }
    return;
  }
  case Number::Type::real: {
    auto d = n->get<Number::real_type>();
    VM.return_value = {(d >= 0) ? n : new Number(-d)};
    return;
  }
  case Number::Type::complex: {
    fprintf(zs::err, complex_found::msg);
    VM.return_value = {};
    return;
  }
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}

template<typename Fun>
inline
void number_divop(const char* name, Fun&& fun){
  auto args = pick_args<2>();
  Number* n[2];

  for(auto i = 0; i < 2; ++i){
    n[i] = args[i].get<Number*>();
    if(!n[i]){
      number_type_check_failed(name, args[i]);
      return;
    }
    if(n[i]->type() != Number::Type::integer){
      fprintf(zs::err, "native func: %s: not integer type (%s)",
              name, stringify(n[i]->type()));
      VM.return_value = {};
      return;
    }
  }
  
  VM.return_value = {new Number{fun(n[0]->get<Number::integer_type>(),
                                    n[1]->get<Number::integer_type>())}};
}

void number_quot(){
  number_divop("quotient", std::divides<Number::integer_type>());
}

void number_rem(){
  number_divop("remainder",
               [](Number::integer_type i1, Number::integer_type i2) -> Number::integer_type{
                 auto q = i1 / i2;
                 return i1 - (q * i2);
               });
}

void number_mod(){
  number_divop("modulo", 
               [](Number::integer_type i1, Number::integer_type i2) -> Number::integer_type{
                 auto m = i1 % i2;

                 if((m < 0 && i2 > 0) || (m > 0 && i2 < 0)){
                   return m + i2;
                 }else{
                   return m;
                 }
               });
}

template<typename T>
T gcd(T m, T n){
  if(m < 0) m = -m;
  if(n < 0) n = -n;

  if(m < n)
    std::swap(m, n);

  while(n > 0){
    auto mod = m % n;
    m = n;
    n = mod;
  }

  return m;
}

void number_gcd(){
  number_accumulate("gcd", Number(0l),
                    [](Number& n1, const Number& n2) -> bool {
                      if(n1.type() != Number::Type::integer || n2.type() != Number::Type::integer){
                        fprintf(zs::err, "native func: gcd: not integer passed.\n");
                        return false;
                      }

                      auto i1 = n1.get<Number::integer_type>();
                      auto i2 = n2.get<Number::integer_type>();

                      n1 = Number{gcd(i1, i2)};
                      return true;
                    });
}

void number_lcm(){
  number_accumulate("lcm", Number(1l),
                    [](Number& n1, const Number& n2) -> bool {
                      if(n1.type() != Number::Type::integer || n2.type() != Number::Type::integer){
                        fprintf(zs::err, "native func: gcd: not integer passed.\n");
                        return false;
                      }

                      auto i1 = n1.get<Number::integer_type>();
                      auto i2 = n2.get<Number::integer_type>();

                      n1 = Number{abs(i1 * i2 / gcd(i1, i2))};
                      return true;
                    });
}

void number_numerator(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    number_type_check_failed("numerator", arg);
    return;
  }

  fprintf(zs::err, "native func: 'numerator' is not implemented.\n");
  VM.return_value = {};
}

void number_denominator(){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    number_type_check_failed("denominator", arg);
    return;
  }

  fprintf(zs::err, "native func: 'denominator' is not implemented.\n");
  VM.return_value = {};
}


template<typename Fun>
inline
void number_rounding(const char* name, Fun&& fun){
  auto arg1 = pick_args_1();

  auto n = arg1.get<Number*>();
  if(!n){
    number_type_check_failed(name, arg1);
    return;
  }

  switch(n->type()){
  case Number::Type::integer:
    VM.return_value = {n};
    return;
  case Number::Type::real:
    VM.return_value = {new Number(fun(n->get<Number::real_type>()))};
    return;
  case Number::Type::complex:
    fprintf(zs::err, complex_found::msg);
    VM.return_value = {};
    return;
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}

void number_floor(){
  number_rounding("floor", [](Number::real_type d){ return std::floor(d); });
}

void number_ceil(){
  number_rounding("ceiling", [](Number::real_type d){ return std::ceil(d); });
}

void number_trunc(){
  number_rounding("truncate", [](Number::real_type d){ return std::trunc(d); });
}

void number_round(){
  number_rounding("round", [](Number::real_type d){ return std::round(d); });
}


void number_rationalize(){
  auto args = pick_args<2>();
  Number* n[2];

  for(auto i = 0; i < 2; ++i){
    n[i] = args[i].get<Number*>();
    if(!n[i]){
      number_type_check_failed("rationalize", args[i]);
      return;
    }
  }
  
  fprintf(zs::err, "native func: 'rationalize' is not implemented.\n");
  VM.return_value = {};
}


template<typename Fun>
inline
void number_unary_op(const char* name, Fun&& fun){
  auto arg1 = pick_args_1();

  auto n = arg1.get<Number*>();
  if(!n){
    number_type_check_failed(name, arg1);
    return;
  }

  switch(n->type()){
  case Number::Type::integer:
  case Number::Type::real:
    VM.return_value = {new Number(fun(n->coerce<Number::real_type>()))};
    return;
  case Number::Type::complex:
    VM.return_value = {new Number(fun(n->get<Number::complex_type>()))};
    return;
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}

struct exp_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::exp(t1);
  }
};

void number_exp(){
  number_unary_op("exp", exp_fun());
}

struct log_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::log(t1);
  }
};

void number_log(){
  number_unary_op("log", log_fun());
}

struct sin_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::sin(t1);
  }
};

void number_sin(){
  number_unary_op("sin", sin_fun());
}

struct cos_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::cos(t1);
  }
};

void number_cos(){
  number_unary_op("cos", cos_fun());
}

struct tan_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::tan(t1);
  }
};

void number_tan(){
  number_unary_op("tan", tan_fun());
}

struct asin_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::asin(t1);
  }
};

void number_asin(){
  number_unary_op("asin", asin_fun());
}

struct acos_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::acos(t1);
  }
};

void number_acos(){
  number_unary_op("acos", acos_fun());
}

void number_atan(){
  auto arg1 = VM.stack.top();
  VM.stack.pop();

  auto n1 = arg1.get<Number*>();
  if(!n1){
    number_type_check_failed("atan", arg1);
    clean_args();
    return;
  }

  // std::atan()
  if(VM.stack.top().tag() == Ptr_tag::vm_op){
    VM.stack.pop();

    switch(n1->type()){
    case Number::Type::integer:
    case Number::Type::real:
      VM.return_value = {new Number(std::atan(n1->coerce<Number::real_type>()))};
      return;
    case Number::Type::complex: {
      VM.return_value = {new Number(std::atan(n1->get<Number::complex_type>()))};
      return;
    }
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }
  }

  // std::atan2()
  auto arg2 = pick_args_1();
  auto n2 = arg2.get<Number*>();
  if(!n2){
    number_type_check_failed("atan", arg2);
    return;
  }

  switch(n2->type()){
  case Number::Type::integer:
  case Number::Type::real:
    VM.return_value = {new Number(std::atan2(n1->coerce<Number::real_type>(),
                                             n2->coerce<Number::real_type>()))};
    return;
  case Number::Type::complex:
    fprintf(zs::err, "native func: (atan <complex> <complex>) is not implemented.\n");
    VM.return_value = {};
    return;
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}

struct sqrt_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::sqrt(t1);
  }
};

void number_sqrt(){
  number_unary_op("sqrt", sqrt_fun());
}

template<typename RFun, typename CFun>
void number_binary_op(const char* name, RFun&& rfun, CFun&& cfun){
  auto args = pick_args<2>();
  Number* n[2];

  for(auto i = 0; i < 2; ++i){
    n[i] = args[i].get<Number*>();
    if(!n[i]){
      number_type_check_failed(name, args[i]);
      return;
    }
  }
  
  if(n[0]->type() == Number::Type::uninitialized
     || n[1]->type() == Number::Type::uninitialized){
    UNEXP_DEFAULT();
  }

  if(n[0]->type() <= Number::Type::real
     || n[1]->type() <= Number::Type::real){
    VM.return_value = {new Number(rfun(n[0]->coerce<Number::real_type>(),
                                       n[1]->coerce<Number::real_type>()))};
    return;
  }

  if(n[0]->type() <= Number::Type::complex
     || n[1]->type() <= Number::Type::complex){
    VM.return_value = cfun(n[0]->coerce<Number::complex_type>(),
                           n[1]->coerce<Number::complex_type>());
    return;
  }

  UNEXP_DEFAULT();
}

void number_expt(){
  number_binary_op("expt",
                   [](Number::real_type n1, Number::real_type n2){
                     return std::pow(n1, n2);
                   },
                   [](const Number::complex_type& n1, const Number::complex_type& n2){
                     return Lisp_ptr{new Number(std::pow(n1, n2))};
                   });
}

void number_rect(){
  number_binary_op("make-rectangular",
                   [](Number::real_type n1, Number::real_type n2){
                     return Number::complex_type(n1, n2);
                   },
                   [](const Number::complex_type&, const Number::complex_type&){
                     return Lisp_ptr{};
                   });
}

void number_polar(){
  number_binary_op("make-polar",
                   [](Number::real_type n1, Number::real_type n2){
                     return polar(n1, n2);
                   },
                   [](const Number::complex_type&, const Number::complex_type&){
                     return Lisp_ptr{};
                   });
}


template<typename Fun>
inline
void number_unary_op_complex(const char* name, Fun&& fun){
  auto arg1 = pick_args_1();

  auto n = arg1.get<Number*>();
  if(!n){
    number_type_check_failed(name, arg1);
    return;
  }

  switch(n->type()){
  case Number::Type::integer:
  case Number::Type::real:
  case Number::Type::complex:
    VM.return_value = {new Number(fun(n->coerce<Number::complex_type>()))};
    return;
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}

void number_real(){
  number_unary_op_complex("real-part",
                          [](const Number::complex_type& z){
                            return z.real();
                          });
}

void number_imag(){
  number_unary_op_complex("imag-part",
                          [](const Number::complex_type& z){
                            return z.imag();
                          });
}

void number_mag(){
  number_unary_op_complex("magnitude",
                          [](const Number::complex_type& z){
                            return std::abs(z);
                          });
}

void number_angle(){
  number_unary_op_complex("angle",
                          [](const Number::complex_type& z){
                            return arg(z);
                          });
}

template<typename Fun>
void number_i_e(const char* name, Fun&& fun){
  auto arg1 = pick_args_1();

  auto n = arg1.get<Number*>();
  if(!n){
    number_type_check_failed(name, arg1);
    return;
  }

  VM.return_value = {new Number(fun(*n))};
}

void number_i_to_e(){
  number_i_e("inexact->exact", to_exact);
}

void number_e_to_i(){
  number_i_e("exact->inexact", to_inexact);
}


void number_from_string(){
  auto arg1 = VM.stack.top();
  VM.stack.pop();

  auto str = arg1.get<String*>();
  if(!str){
    fprintf(zs::err, "native func: string->number: passed arg is not string (%s).\n",
            stringify(arg1.tag()));
    clean_args();
    VM.return_value = {};
    return;
  }

  int radix;

  if(VM.stack.top().tag() == Ptr_tag::vm_op){
    VM.stack.pop();
    radix = 10;
  }else{
    auto arg2 = pick_args_1();
    auto num = arg2.get<Number*>();
    if(!num){
      fprintf(zs::err, "native func: string->number: passed radix is not number (%s).\n",
              stringify(arg2.tag()));
      VM.return_value = {};
      return;
    }
    if(num->type() != Number::Type::integer){
      fprintf(zs::err, "native func: string->number: passed radix is not number (%s).\n",
              stringify(arg2.tag()));
      VM.return_value = {};
      return;
    }
    radix = num->get<Number::integer_type>();
  }
    
  auto f = make_string_input_stream(str->c_str(), str->size());
  auto n = parse_number(f, radix);
  fclose(f);

  if(n){
    VM.return_value = {new Number(n)};
  }else{
    VM.return_value = {};
  }
}

void number_to_string(){
  auto arg1 = VM.stack.top();
  VM.stack.pop();

  auto n = arg1.get<Number*>();
  if(!n){
    fprintf(zs::err, "native func: number->string: passed arg is not number (%s).\n",
            stringify(arg1.tag()));
    clean_args();
    VM.return_value = {};
    return;
  }

  int radix;

  if(VM.stack.top().tag() == Ptr_tag::vm_op){
    VM.stack.pop();
    radix = 10;
  }else{
    auto arg2 = pick_args_1();
    auto num = arg2.get<Number*>();
    if(!num){
      fprintf(zs::err, "native func: number->string: passed radix is not number (%s).\n",
              stringify(arg2.tag()));
      VM.return_value = {};
      return;
    }
    if(num->type() != Number::Type::integer){
      fprintf(zs::err, "native func: number->string: passed radix is not number (%s).\n",
              stringify(arg2.tag()));
      VM.return_value = {};
      return;
    }
    radix = num->get<Number::integer_type>();
  }

  char* buf = NULL;
  size_t buf_size = 0;

  auto f = open_memstream(&buf, &buf_size);
  print(f, *n, radix);
  fclose(f);

  if(n){
    VM.return_value = {new String(buf)};
  }else{
    VM.return_value = {};
  }

  free(buf);
}


constexpr BuiltinFunc
builtin_numeric[] = {
  {"number?", {
      type_check_pred<Ptr_tag::number>,
      Calling::function, {1}}},

  {"complex?", {
      complexp,
      Calling::function, {1}}},
  {"real?", {
      realp,
      Calling::function, {1}}},
  {"rational?", {
      rationalp,
      Calling::function, {1}}},
  {"integer?", {
      integerp,
      Calling::function, {1}}},

  {"exact?", {
      exactp,
      Calling::function, {1}}},
  {"inexact?", {
      inexactp,
      Calling::function, {1}}},

  {"=", {
      number_equal,
      Calling::function, {2, Variadic::t}}},
  {"<", {
      number_less,
      Calling::function, {2, Variadic::t}}},
  {">", {
      number_greater,
      Calling::function, {2, Variadic::t}}},
  {"<=", {
      number_less_eq,
      Calling::function, {2, Variadic::t}}},
  {">=", {
      number_greater_eq,
      Calling::function, {2, Variadic::t}}},

  {"zero?", {
      zerop,
      Calling::function, {1}}},
  {"positive?", {
      positivep,
      Calling::function, {1}}},
  {"negative?", {
      negativep,
      Calling::function, {1}}},
  {"odd?", {
      oddp,
      Calling::function, {1}}},
  {"even?", {
      evenp,
      Calling::function, {1}}},

  {"max", {
      number_max,
      Calling::function, {2, Variadic::t}}},
  {"min", {
      number_min,
      Calling::function, {2, Variadic::t}}},

  {"+", {
      number_plus,
      Calling::function, {0, Variadic::t}}},
  {"*", {
      number_multiple,
      Calling::function, {0, Variadic::t}}},
  {"-", {
      number_minus,
      Calling::function, {1, Variadic::t}}},
  {"/", {
      number_divide,
      Calling::function, {1, Variadic::t}}},

  {"abs", {
      number_abs,
      Calling::function, {1}}},

  {"quotient", {
      number_quot,
      Calling::function, {2}}},
  {"remainder", {
      number_rem,
      Calling::function, {2}}},
  {"modulo", {
      number_mod,
      Calling::function, {2}}},

  {"gcd", {
      number_gcd,
      Calling::function, {0, Variadic::t}}},
  {"lcm", {
      number_lcm,
      Calling::function, {0, Variadic::t}}},

  {"numerator", {
      number_numerator,
      Calling::function, {1}}},
  {"denominator", {
      number_denominator,
      Calling::function, {1}}},

  {"floor", {
      number_floor,
      Calling::function, {1}}},
  {"ceiling", {
      number_ceil,
      Calling::function, {1}}},
  {"truncate", {
      number_trunc,
      Calling::function, {1}}},
  {"round", {
      number_round,
      Calling::function, {1}}},

  {"rationalize", {
      number_rationalize,
      Calling::function, {1}}},

  {"exp", {
      number_exp,
      Calling::function, {1}}},
  {"log", {
      number_log,
      Calling::function, {1}}},
  {"sin", {
      number_sin,
      Calling::function, {1}}},
  {"cos", {
      number_cos,
      Calling::function, {1}}},
  {"tan", {
      number_tan,
      Calling::function, {1}}},
  {"asin", {
      number_asin,
      Calling::function, {1}}},
  {"acos", {
      number_acos,
      Calling::function, {1}}},
  {"atan", {
      number_atan,
      Calling::function, {1, Variadic::t}}},

  {"sqrt", {
      number_sqrt,
      Calling::function, {1}}},
  {"expt", {
      number_expt,
      Calling::function, {2}}},

  {"make-rectangular", {
      number_rect,
      Calling::function, {2}}},
  {"make-polar", {
      number_polar,
      Calling::function, {2}}},
  {"real-part", {
      number_real,
      Calling::function, {1}}},
  {"imag-part", {
      number_imag,
      Calling::function, {1}}},
  {"magnitude", {
      number_mag,
      Calling::function, {1}}},
  {"angle", {
      number_angle,
      Calling::function, {1}}},

  {"inexact->exact", {
      number_i_to_e,
      Calling::function, {1}}},
  {"exact->inexact", {
      number_e_to_i,
      Calling::function, {1}}},

  {"string->number", {
      number_from_string,
      Calling::function, {1, Variadic::t}}},
  {"number->string", {
      number_to_string,
      Calling::function, {1, Variadic::t}}}
};

} //namespace

void install_builtin_numeric(){
  for(auto& e : builtin_numeric){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
